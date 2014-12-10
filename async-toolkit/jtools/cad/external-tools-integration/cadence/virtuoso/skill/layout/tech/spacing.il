; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun SpacingGetSpacingRuleName ( SpacingRule )
  ( car SpacingRule ) )

(defun SpacingGetSpacingRuleSpacing ( SpacingRule )
  ( nth 1 SpacingRule ) )

(defun SpacingGetSpacingRuleLayers ( SpacingRule )
  (if ( not ( SpacingIsSameLayerSpacingRule SpacingRule ) )
      ( list
        ( nth 2 SpacingRule )
        ( nth 3 SpacingRule ) )
    ( list
      ( nth 2 SpacingRule ) ) ) )

(defun  SpacingIsSameLayerSpacingRule ( SpacingRule )
  ( null ( nth 3 SpacingRule ) ) )


(defun SpacingGetSpacingRulesForLayer ( CellView LayerPurposePair RuleName )
  (let (
        ( LayerName ( car LayerPurposePair ) )
        ( PurposeName ( car ( cdr LayerPurposePair ) ) )
        ( TechFileData ( techGetTechFile CellView ) ) )
    (when TechFileData
      ( ListApplyFuncToListAndAccumulateNonNilResults
        ( techGetSpacingRules TechFileData )
        (lambda
          ( Rule LayerName )
          (let (
                ( RuleLayers ( SpacingGetSpacingRuleLayers Rule ) ) )
            (when ( and
                    ( or
                      ( and
                        ( not ( SpacingIsSameLayerSpacingRule Rule ) )
                        ( equal LayerName ( car ( cdr RuleLayers ) ) ) )
                      ( equal LayerName ( car RuleLayers ) ) )
                    ( equal RuleName ( SpacingGetSpacingRuleName Rule ) ) )
              Rule ) ) )
        ( list
          LayerName ) ) ) ) )


(defun SpacingGetMinSpacingRulesForLayer ( CellView LayerPurposePair )
  ( SpacingGetSpacingRulesForLayer CellView LayerPurposePair "minSpacing" ) )

(defun SpacingGetMinWidthRuleMinWidth ( Rule )
  ( nth 1 Rule ) )

(defun SpacingGetMinWidthRuleForLayer ( CellView LayerPurposePair )
  ( car ( SpacingGetSpacingRulesForLayer CellView LayerPurposePair "minWidth" ) ) )

(defun SpacingGetMinWidthForLayer ( CellView LayerPurposePair )
  ( SpacingGetMinWidthRuleMinWidth 
    ( SpacingGetMinWidthRuleForLayer CellView LayerPurposePair ) ) )

(defun SpacingGetSameLayerSpacingRuleForLayer ( CellView LayerPurposePair )
  (let (
        ( SpacingRules ( SpacingGetMinSpacingRulesForLayer CellView LayerPurposePair ) )
        )
    ( ListFindElement
      SpacingRules
      (lambda
        ( Rule )
        ( SpacingIsSameLayerSpacingRule Rule ) )
      nil ) ) )

(defun SpacingGetSameLayerSpacingForLayer ( CellView LayerPurposePair )
  (let (
        ( Rule ( SpacingGetSameLayerSpacingRuleForLayer CellView LayerPurposePair ) ) )
    (when Rule
      ( SpacingGetSpacingRuleSpacing
        Rule ) ) ) )


(defun SpacingGetLayerSpacingForLayers ( CellView LPP1 LPP2 )
  ( nth
    1
    ( ListFindElement
      ( SpacingGetMinSpacingRulesForLayer
        CellView
        LPP1 )
      (lambda
        ( Rule )
        ( or
          ( and
            ( equal ( nth 2 Rule ) ( car LPP1 ) )
            ( equal ( nth 3 Rule ) ( car LPP2 ) ) ) 
          ( and
            ( equal ( nth 3 Rule ) ( car LPP1 ) )
            ( equal ( nth 2 Rule ) ( car LPP2 ) ) ) 
          ) )
      nil ) ) )
