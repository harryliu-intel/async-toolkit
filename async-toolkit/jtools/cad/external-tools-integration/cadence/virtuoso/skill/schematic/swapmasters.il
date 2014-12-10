; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun SwapSchematicTransistorMasters ( TargetCellView TechLibName NTransistorName PTransistorName )
  ( SwapSchematicMasters
    TargetCellView
    ( list
      ( list
        ( list
          TechLibName
          "nfet"
          "symbol"
          )
        ( list
          TechLibName
          NTransistorName
          "symbol"
          "schematicSymbol"
          )
        )
      ( list
        ( list
          TechLibName
          "pfet"
          "symbol"
          )
        ( list
          TechLibName
          PTransistorName
          "symbol"
          "schematicSymbol"
          )
        )
      )
    )
  )

(defun SwapSchematicMasters ( TargetCellView Mappings )

  (let (
        ( ReplaceParamsTable ( makeTable "FooTable" nil ) )
        ( MappingTable ( makeTable "FooTable" nil ) )
        )
    ( foreach
      Mapping
      Mappings
      (let (
            ( ExistingTripple ( car Mapping ) )
            ( NewQuadruple ( cadr Mapping ) )
            ( ReplaceParams ( caddr Mapping ) )
            )
        (let (
              ( ExistingLibName ( car ExistingTripple ) )
              ( ExistingCellName ( nth 1 ExistingTripple ) )
              ( ExistingViewName ( nth 2 ExistingTripple ) )
              ( NewLibName ( car NewQuadruple ) )
              ( NewCellName ( nth 1 NewQuadruple ) )
              ( NewViewName ( nth 2 NewQuadruple ) )
              ( NewViewType ( nth 3 NewQuadruple ) )
              )
          (when ( ddGetObj 
                  NewLibName NewCellName NewViewName )
            (let (
                  ( Key 
                    ( list ExistingLibName ExistingCellName ExistingViewName ) )
                  ( NewCellView 
                    ( dbOpenCellViewByType 
                      NewLibName NewCellName NewViewName NewViewType "r" ) )
                  )
              ( setarray ReplaceParamsTable Key ReplaceParams )
              (when NewCellView
                ( setarray MappingTable Key NewCellView ) )
              ) ) ) ) )
    
    ( foreach
      Instance
      ( getq TargetCellView instances )
      (let (
            ( Key 
              ( list 
                ( getq
                  Instance
                  libName
                  )
                ( getq
                  Instance
                  cellName
                  )
                ( getq
                  Instance
                  viewName
                  )
                ) ) )
        (let (
              ( NewCellView ( arrayref MappingTable Key ) )
              ( Params (if ( not ( arrayref ReplaceParamsTable Key ) )
                           ( PCellGetParameterList Instance ) ) )
              )
          (when NewCellView
            ( dbSetq Instance NewCellView master )
            )
          (when Params
            ( PCellSetParameterList Instance Params ) )
          ) ) ) ) )


(defun SwapInstanceMasterViews ( TargetCellView DestViewName DestViewType )
  (let (
        ( MasterMap ( makeTable `master nil ) ) )
    ( foreach Instance ( getq TargetCellView instances )
              (let (
                    ( LibName ( getq Instance libName ) )
                    ( CellName ( getq Instance cellName ) )
                    ( ViewName ( getq Instance viewName ) ) )
                (if ( not ( arrayref MasterMap ( list LibName CellName ViewName ) ) )
                    (if ( ddGetObj LibName CellName DestViewName )
                        ( setarray 
                          MasterMap
                          ( list LibName CellName ViewName ) 
                          ( list LibName CellName DestViewName DestViewType ) ) ) ) ) )
    ( SwapSchematicMasters TargetCellView ( tableToList MasterMap ) ) ) )
