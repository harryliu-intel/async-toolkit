; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun FileUtilAppendFile ( OutPort InFileName )
  (let (
        ( InFilePort ( infile InFileName ) )
        ( Line nil )
        )
    (while ( and ( portp InFilePort )
                 ( gets Line InFilePort )  )
      ( fprintf OutPort "%s" Line ) )
  )
)


(defun FileUtilReadExpressionFromFile ( InFileName )
  (let (
	 ( InFilePort ( infile InFileName ) )
        )
    (let (
          ( Ret ( read InFilePort ) ) )
    ( close InFilePort )
    Ret
    ) ) )

(defun FileUtilAppendStringToFileAndClose ( FileName String )
  (let (
	 ( OutPort ( outfile FileName "a" ) )
        )
    ( fprintf OutPort String )
    ( close OutPort ) ) )


(defun FileUtilDirFiles ( DirName )
  ( mapcar
    (lambda ( FileName )
      ( strcat DirName "/" FileName ) )
    ( getDirFiles DirName ) ) )


; returns 0 if directory created
; returns 1 if MakeParents was nil and parent directory did not exist.
; returns nil if createDir returns nil.
(defun FileUtilMakeDir ( DirName
                         @optional
                         ( MakeParents nil ) )
  (let (
        ( PathParts ( PathGetPathComponents
                      ( PathMakePathAbsolute
                        DirName ) ) )
        ( CurrPath "" )
        ( Ret 0 ) )
    (while ( and
             ( equal Ret 0 )
             PathParts )
      ( setq CurrPath ( strcat CurrPath ( strcat "/" ( car PathParts ) ) ) )
      ( setq PathParts ( cdr PathParts ) )
      (unless ( isDir CurrPath )
        (if MakeParents
            (unless ( createDir CurrPath )
              ( setq Ret nil ) )
          ( setq Ret 1 ) ) ) )
    Ret ) )
