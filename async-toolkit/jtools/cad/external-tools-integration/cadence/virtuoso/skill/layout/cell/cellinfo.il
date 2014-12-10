; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun CellInfoGetDirectivesSkillFileForCellName ( CellName DirectivesSkillDir )
  ( sprintf nil "%s/%s.directives.il" DirectivesSkillDir CellName ) )


(defun CellInfoGetTableForCellName ( CellName DirectivesSkillDir )
  (let (
        ( DirectivesSkillFileName ( CellInfoGetDirectivesSkillFileForCellName
                                    CellName
                                    DirectivesSkillDir ) ) )
    ( CellInfoGetTableForFileName
      DirectivesSkillFileName ) ) )

(defun CellInfoGetTableForFileName ( DirectivesSkillFileName )
 (if ( isReadable
          DirectivesSkillFileName )
        (let ()
          (defvar DirectiveTable nil )
          ( load DirectivesSkillFileName )
          (if DirectiveTable
              DirectiveTable 
            ( sprintf
              nil
              "Could not load %L even though it is readable."
              DirectivesSkillFileName ) ) )
      ( sprintf
        nil
        "%L is not a readable file."
        DirectivesSkillFileName ) ) )


(defun CellInfoGetDF2Path ( LibName CellName )
 (let (
	( CellDirObj ( ddGetObj LibName CellName ) )
       )
    ( cond (
	    ( null CellDirObj )
	    ( printf "CellName/LibName not valid\n" )
	    nil
	    )
	   (
	    ( ddGetObjWritePath CellDirObj)   	    
	   )
    )
  )
)


(defun CellInfoGetSubTable ( Table SubTableKey )
  ( arrayref Table SubTableKey ) )

(defun CellInfoLookup ( Table Key )
  ( arrayref Table Key )
)

(defun CellInfoLookupInBlock ( Table Block Key )
  (let (
	( BlockTable 
	  ( arrayref Table Block ) ) )
    (when ( tablep BlockTable  )
      ( CellInfoLookup BlockTable Key ) ) ) )

(defun CellInfoLookupParametrizedInBlock ( Table Block Key Type Parameter )
  (let (
	( BlockTable 
	  ( arrayref Table Block ) ) )
    (when ( tablep BlockTable )
      ( CellInfoLookupParametrized BlockTable Key Type Parameter ) ) ) )

(defun CellInfoLookupTable ( Table Key Type )
  ( arrayref Table ( sprintf nil "%s(%s)" Key Type ) ) )

(defun CellInfoLookupParametrized ( Table Key Type Parameter )
  (let (
	( SubTable ( CellInfoLookupTable Table Key Type ) ) )
    (when ( tablep SubTable )
      ( arrayref SubTable Parameter ) ) ) )

(defun CellInfoGetHeightInMeters ( Table DirectiveUnitsPerMeter )
  ( quotient 
    ( times ( float ( CellInfoLookup Table "bitpitch" ) ) 
            ( float ( CellInfoLookup Table "height" ) ) )
    DirectiveUnitsPerMeter ) )

(defun CellInfoGetPinLayer ( Table )
  ( CellInfoLookup Table "pinlayer" )
)

(defun CellInfoGetNetWireWidthInMeters ( Table NetName DirectiveUnitsPerMeter )
  (let (
        ( Ret
          (cond (
                 ( CellInfoLookupParametrizedInBlock Table "subcell" "wirewidth" "node" NetName ) )
                (
                 ( CellInfoLookupInBlock Table "subcell" "wirewidth" ) )
                (
                 ( CellInfoLookupParametrized Table "wirewidth" "node" NetName ) )
                (
                 ( CellInfoLookup Table "wirewidth" )
                 ) ) ) )
    (when Ret
      ( quotient
        Ret
        DirectiveUnitsPerMeter ) ) ) )

(defun CellInfoGetNetWireSpacingInMeters ( Table NetName DirectiveUnitsPerMeter )
  (let (
        ( Ret
          (cond (
                 ( CellInfoLookupParametrizedInBlock Table "subcell" "wirespace" "node" NetName ) )
                (
                 ( CellInfoLookupInBlock Table "subcell" "wirespace" ) )
                (
                 ( CellInfoLookupParametrized Table "wirespace" "node" NetName ) )
                (
                 ( CellInfoLookup Table "wirespace" ) ) ) ) )
    (when Ret
      ( quotient
        Ret
        DirectiveUnitsPerMeter ) ) ) )

(defun CellInfoGetLayerWirePitchInMeters ( Table Layer DirectiveUnitsPerMeter )
  ( quotient
    ( CellInfoLookupParametrized Table "layer_wirepitch" "layer" Layer )
     DirectiveUnitsPerMeter ) )

(defun CellInfoGetLayerWireSpacingInMeters ( Table Layer DirectiveUnitsPerMeter )
 ( quotient 
   ( CellInfoLookupParametrized Table "layer_wirespacing" "layer" Layer )
    DirectiveUnitsPerMeter ) )

;FIXME
(defun CellInfoGetLayerPowerGridWireWidthInMeters ( Table Layer DirectiveUnitsPerMeter )
  ( quotient
    (cond (
           ( CellInfoLookupParametrized Table "layer_powergrid_wirewidth" "layer" Layer ) )
          (
           0.72e-6 ) )
    DirectiveUnitsPerMeter ) )

(defun CellInfoGetLayerWireWidthInMeters ( Table Layer DirectiveUnitsPerMeter )
  ( quotient
    ( CellInfoLookupParametrized Table "layer_wirewidth" "layer" Layer )
    DirectiveUnitsPerMeter ) )

(defun CellInfoGetLayerKeepOutPattern ( Table MetalLPP )
  ( CellInfoLookupParametrized Table "layer_keepout_pattern" "layer" MetalLPP ) )
