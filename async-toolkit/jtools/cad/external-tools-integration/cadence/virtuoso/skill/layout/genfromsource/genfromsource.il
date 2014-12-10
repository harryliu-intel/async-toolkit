; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: GenFromSource
; Parameter: LibName (string)
;            CellName (string)
;            ViewName (string)
;            SkillNetListDir (list)
;            SkillDirectivesDir (list)
; Return: GenFromSourceView (CVId)
;
; Takes inputs from SkillNetListDir &  SkillDirectivesDir. Create a new
; cell view with LibName CellName ViewName. Create Pcell instances 
; according to SkillNetListDir &  SkillDirectivesDir in this newly created
; cell view.

defun( GenFromSourceMain ( LibName CellName ViewName
            SkillNetlistDir SkillDirectivesDir)

  GenFromSourceView = dbOpenCellViewByType( 
                   LibName 
                   CellName
                   ViewName
                   "maskLayout" 
                   "w" )

  SyncNetlistGenFromSkillNetlistUsingPDKInfo( 
    GenFromSourceView
    SkillNetlistDir 
    SkillDirectivesDir
  )

  dbSave( GenFromSourceView )
  GenFromSourceView
 )

(defun GenFromSource ( ConnectivityCellView 
                       TargetCellView
                       GateLibCellRegExs
                       NChainLibCellRegExs
                       PChainLibCellRegExs
                       SymbolicViewName
                       LayoutViewName )


 (let (
       ( NChains ( cadr ( NameFilterInstances 
                              ( getq CellView instances )
                              NChainLibCellRegExs ) ) )
       ( PChains ( cadr ( NameFilterInstances 
                                 ( getq CellView instances )
                                 PChainLibCellRegExs ) ) )
       ( Gates ( cadr ( NameFilterInstances 
                        ( getq CellView instances )
                        GateLibCellRegExs ) ) ) )

   ( foreach Instance ( append Gates NChains PChains )
             ( GenFromSourceCreateLayoutGateFromSymbolGate               
               TargetCellView               
               Instance
               LayoutViewName
               ) ) ) )
 

(defun GenFromSourceCreateLayoutGateFromSymbolGate ( TargetCellView
                                                     Gate
                                                     LayoutViewName )
  (let (
        ( Master ( getq Gate master ) ) )
    ( dbCreateParamInstByMasterName   
      TargetCellView
      ( getq Master libName )
      ( getq Master cellName )
      LayoutViewName
      ( getq Gate name )
      ( getq Gate xy )
      ( getq Gate orient )
      1
      ( PCellGetPropertyList Gate ) ) ) )




