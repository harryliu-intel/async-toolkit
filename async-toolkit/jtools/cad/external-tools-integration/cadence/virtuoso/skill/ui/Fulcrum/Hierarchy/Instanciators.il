; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "Get Instantiators" "UIGetInstantiators" )

;(when ( geqp ( atoi ( VersionGetMajorVersion ) ) 5.0 )
(when t
  ( UICreateMenuItemCallBack
    `UIGetInstantiatorsCB
    (lambda ()
      ( hiDisplayForm UIGetInstantiators_Form ) ) )

  ( UICreateComponent
    "UIGetInstantiators_Form"
    ( UICreateConstructor
      (lambda ( Children )
        (let (
              ( Fields
                ( list,
                  ( list
                    ( hiCreateListBoxField
                      ?name `UIGetInstantiators_Instantiators
                      ?prompt "Instantiators" )
                    ( list
                      0 0 )
                    ( list
                      500 500 )
                    100 )
                  ( list ( hiCreateButton
                           ?name `UIGetInstantiators_Refresh
                           ?buttonText "Refresh"
                           ?callback "( UIGetInstantiatorsFormMapCB )"
                           )
                         ( list 0 20 )
                         ( list 100 20 )
                         100 )
                  ) ) )
          ( defvar UICellsInstantiatorsTable nil )
          ( hiCreateAppForm
            ?name `UIGetInstantiators_Form
            ?formTitle "Instantiators"
            ?fields Fields
            ?attachmentList ( list 
                              ( bor 
                                hicTopPositionSet 
                                hicLeftPositionSet
                                hicRightPercentSet
                                hicBottomPercentSet )
                              ( bor 
                                hicTopPositionSet 
                                hicLeftPositionSet
                                hicRightPercentSet
                                hicBottomPercentSet )
                              )
            ?initialSize ( list 500 500 )
            ?callback "( UIGetInstantiatorsFormCB )"
            ?mapCB "( UIGetInstantiatorsFormMapCB )" ) ) ) ) )

  ( UICreateAction
    `UIGetInstantiatorsFormCB
    (lambda ()
      (let (
            ( CellNameToOpen ( car 
                               ( getq
                                 ( getq
                                   UIGetInstantiators_Form
                                   UIGetInstantiators_Instantiators )
                                 value ) ) ) )
        (let (
              ( CurrCellView ( geGetWindowCellView ) )
              ( LibCellPair ( NameParseCellName CellNameToOpen ) ) )
          (let (
                ( DEOpenParam ( list
                                nil
                                'libName ( car LibCellPair )
                                'cellName ( cadr LibCellPair )
                                'viewName "floorplan" ) ) )
            ( deOpen DEOpenParam nil "r" )
            (let (
                  ( OpenedCellView ( geGetWindowCellView ) ) )
              ( geDeselectAll )
              ( foreach
                Instance
                ( getq OpenedCellView instances )
                (when ( and
                        ( equal 
                          ( getq Instance cellName )
                          ( getq CurrCellView cellName ) )
                        ( equal
                          ( getq Instance libName )
                          ( getq CurrCellView libName ) ) )
                  ( geSelectObject Instance ) ) ) ) ) ) ) ) )

  ( UICreateAction
    `UIGetInstantiatorsFormMapCB
    (lambda ()
      (let (
            ( CellView ( UIGetCellView ) ) )
        ( hiChangeFormTitle
          UIGetInstantiators_Form
          ( sprintf
            nil
            "Instantiators of %s"
            ( getq CellView cellName ) ) )
        ( putpropq
          ( getq
            UIGetInstantiators_Form
            UIGetInstantiators_Instantiators )
          ( sprintf
            nil
            "Instantiators of %s"
            ( getq CellView cellName ) )
          prompt )
        (unless UICellsInstantiatorsTable
          ( setq UICellsInstantiatorsTable
                 ( HierarchyMakeCellsInstantiatorsTable
                   "floorplan"
                   "floorplan"
                   nil ) ) )
        ( putpropq
          ( getq
            UIGetInstantiators_Form
            UIGetInstantiators_Instantiators )
          ( HierarchyGetInstantiatorsOfCell
            UICellsInstantiatorsTable
            ( getq CellView cellName ) )
          choices ) ) ) )

)                             
