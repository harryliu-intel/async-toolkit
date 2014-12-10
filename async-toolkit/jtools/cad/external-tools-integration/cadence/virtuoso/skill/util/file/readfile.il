; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun ReadFileReadFileLinesFromPort ( Input )
  (let (
        ( ReadRet t )
        ( CurrLine nil)
        ( Ret nil ) )
    (while ReadRet
      ( setq ReadRet ( gets CurrLine Input ) )
      (when ReadRet
        (let (
              ( CleanCurrLine ( StringUtilChompNewline CurrLine ) ) )
          (if Ret
              ( tconc Ret CleanCurrLine )
            ( setq Ret ( tconc nil CleanCurrLine ) ) ) ) ) )
    ( car Ret ) ) )

(defun ReadFileReadFileLines ( FileName )
   (let (
         ( Input ( infile FileName ) ) )
     (let (
           ( Lines ( ReadFileReadFileLinesFromPort Input ) ) )
       ( close Input )
       Lines ) ) )
