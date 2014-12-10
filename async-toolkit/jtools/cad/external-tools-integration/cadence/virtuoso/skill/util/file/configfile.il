; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun ConfigFileAppendKeyValuePairsToString ( String CDSP4ConfigTable Keys )
  (let ( 
        ( RetString String ) )
    ( foreach Key Keys
              ( setq RetString 
                     ( sprintf 
                       nil 
                       "%s --%s=%s"
                       RetString
                       Key
                       ( ConfigFileGetValue CDSP4ConfigTable Key ) )  ) )
    RetString ) )
                                       

(defun ConfigFileGetValue ( ConfigTable Key )
  (cond (
         ( arrayref ConfigTable Key ) )
        ( 
         t
         "" ) ) )

(defun ConfigFileParseConfigFileToTable ( ConfigTable ConfigFile )
  (let ( 
        ( Lines ( ReadFileReadFileLines ConfigFile ) ) )
    ( foreach 
      Line
      Lines
      (unless ( or
                ( equal Line "^[ \t\n]$" ) ;empty
                ( rexMatchp "^[ \t\n]*#" Line ) ;comments 
                )
        (let (
              ( ParsedLine ( parseString Line "=" ) ) )
          (let (
                ( Key ( car ParsedLine ) )
                ( Value ( buildString ( cdr ParsedLine ) "=" ) ) )
            (cond (
                   ( equal Key "config" )
                   ( ConfigFileParseConfigFileToTable 
                     ConfigTable 
                     ( PathGetPathForFile ( PathDirName ConfigFile ) Value ) ) )
                  (
                   ( setarray ConfigTable Key Value ) ) ) ) ) ) ) ) )

(defun ConfigFileGetShellEnv ( ConfigTable )
  (let (
        ( EnvDumpFile ( TempFileGetTempFileName "envdump" ) ) )
    ( shell ( sprintf nil "env > %s" EnvDumpFile ) )
    ( ConfigFileParseConfigFileToTable ConfigTable EnvDumpFile )
    ( shell ( sprintf nil "rm %s" EnvDumpFile ) ) ) )
                
                 
