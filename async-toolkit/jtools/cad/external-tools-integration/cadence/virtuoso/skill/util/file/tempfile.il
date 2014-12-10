; Copyright 2003 Fulcrum Microsy\stems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun TempFileInnerGetTempFileName ( NamePrefix )
  (let (
	( TempDirName ( getTempDir ) )
	)
    (let (
	  ( CurrNum 0 )
	  ( CurrName ( sprintf nil "%s/%s%d" TempDirName NamePrefix 0 ) )
	  )
      (while ( isFileName CurrName )
	( setq CurrNum ( plus CurrNum 1 ) )
	( setq CurrName ( sprintf nil "%s/%s%d" TempDirName NamePrefix CurrNum ) )
	)
      CurrName
      )
    )
  )

(defun TempFileGetOutputPort ( NamePrefix )
  (let (
	( Port
	  ( outfile ( TempFileInnerGetTempFileName
		      NamePrefix
		      )
		    "w"
		    )
	  )
	)
    Port
    )
  )

(defun TempFileGetTempFileName ( NamePrefix )
  (let (
	( TempFileName ( TempFileInnerGetTempFileName
			 NamePrefix
			 )
		       )
	)
    (let (
	  ( Port
	    ( outfile TempFileName "w" )
	    )
	  )
      ( close
	Port
	)
      TempFileName
      )
    )
  )
