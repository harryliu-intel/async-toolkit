; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun LVSNodesMakeEmptyLVSNodesTable ( cellName )
  (let (
        ( NewLVSNodesTable ( makeTable "lvsNodesTable" nil )  ) )
    ( setarray NewLVSNodesTable "cn" cellName )
    ( setarray NewLVSNodesTable "i" ( makeTable "lvsNodesInstancesTable" nil ) )
    NewLVSNodesTable ) )


(defun LVSNodesGetCellName ( LVSNodesTable )
  ( arrayref LVSNodesTable "cn" ) )

(defun LVSNodesGetLVSNodesInstancesTable ( LVSNodesTable )
  ( arrayref LVSNodesTable "i" ) )

(defun LVSNodesGetLVSNodes ( LVSNodesTable )
  ( arrayref LVSNodesTable "n" ) )

(defun LVSNodesMakeLVSNodesInstance ( LVSNodesTable InstanceName )
  (let (
        ( LVSNodesInstancesTable ( LVSNodesGetLVSNodesInstancesTable LVSNodesTable ) )
        ( LVSNodesInstance ( makeTable "lvsNodesInstance" nil ) ) )
    ( setarray
      LVSNodesInstance
      "n"
      InstanceName )
    ( setarray
      LVSNodesInstance
      "c"
      ( makeTable "lvsNodeInstanceConnections" nil ) )
    ( setarray
      LVSNodesInstancesTable
      InstanceName
      LVSNodesInstance )
    LVSNodesInstance ) )

(defun LVSNodesGetLVSNodesInstance ( LVSNodesTable InstanceName )
  ( arrayref
    ( LVSNodesGetLVSNodesInstancesTable LVSNodesTable )
    InstanceName ) )

(defun LVSNodesGetLVSNodesInstanceName ( LVSNodeInstance )
  ( arrayref LVSNodesInstance "n" ) )

(defun LVSNodesGetLVSNodesInstanceConnections ( LVSNodesInstance )
  ( arrayref LVSNodesInstance "c" ) )

(defun LVSNodesMakeLVSNodesInstanceConnection ( LVSNodesInstance Node NodeInInstance )
  ( setarray
    ( LVSNodesGetLVSNodesInstanceConnections
      LVSNodesInstance )
    Node
    NodeInInstance ) )

(defun LVSNodesAddLVSNode ( LVSNodesTable NodeName )
  ( setarray
    LVSNodesTable
    "n"
    ( cons
      NodeName
      ( LVSNodesGetLVSNodes LVSNodesTable ) ) ) )

(defun LVSNodesGetLVSNodesInstanceConnectionPairs ( LVSNodesInstance )
  ( tableToList
    ( LVSNodesGetLVSNodesInstanceConnections
      LVSNodesInstance ) ) )
