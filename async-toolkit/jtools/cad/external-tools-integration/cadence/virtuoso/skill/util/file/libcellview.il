; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$



;Code for reading a file of library, cell, view tripples in to a list.




(defun LibCellViewReadLibCellViewTripplesFromPort ( InputPort )
  (let (
        ( FileLines ( ReadFileReadFileLinesFromPort
                      InputPort ) ) )
    ( rexComile "#.*" )
    ( ListApplyFuncToListAndAccumulateNonNilResults
      FileLines
      (lambda
        ( CurrLine )
        (let (
              ( NewCurrLine ( rexReplace CurrLine "" -1 ) ) )
          (let (
                ( Components ( setof
                               Component
                               ( parseString NewCurrLine " " )
                               ( not ( equal Component "" ) ) ) ) )
            (when ( equal ( length Components ) 3 )
              Components ) ) ) )
      nil ) ) )

(defun LibCellViewReadLibCellViewTripplesFromFile ( FileName )
  (let (
        ( InputPort ( infile FileName ) ) )
    (let (
          ( LibCellViewTripples ( LibCellViewReadLibCellViewTripplesFromPort InputPort ) ) )
      ( close InputPort )
      LibCellViewTripples ) ) )


(defun LibCellViewReadLibCellPairsFromPort ( InputPort )
  (let (
        ( FileLines ( ReadFileReadFileLinesFromPort
                      InputPort ) ) )
    ( rexCompile "#.*" )
    ( ListApplyFuncToListAndAccumulateNonNilResults
      FileLines
      (lambda
        ( CurrLine )
        (let (
              ( NewCurrLine ( rexReplace CurrLine "" -1 ) ) )
          (let (
                ( Components ( setof
                               Component
                               ( parseString NewCurrLine " " )
                               ( not ( equal Component "" ) ) ) ) )
            (when ( equal ( length Components ) 2 )
              Components ) ) ) )
      nil ) ) )

(defun LibCellViewReadLibCellPairsFromFile ( FileName )
  (let (
        ( InputPort ( infile FileName ) ) )
    (let (
          ( LibCellPairs ( LibCellViewReadLibCellPairsFromPort InputPort ) ) )
      ( close InputPort )
      LibCellPairs ) ) )
                
