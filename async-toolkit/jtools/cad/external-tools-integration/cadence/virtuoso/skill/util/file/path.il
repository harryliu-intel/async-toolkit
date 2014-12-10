; Copyright 2003 Fulcrum Microsy\stems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun PathIsAbsolutePath ( PathStr )
  ( equal ( substring PathStr 1 1 ) "/" ) )

(defun PathIsRelativeToHome ( PathStr )
  ( equal ( substring PathStr 1 1 ) "~" ) )

(defun PathGetPathComponents ( PathStr )
  (if ( PathIsAbsolutePath PathStr )
      (cons "" ( parseString PathStr "/" ) )
    ( parseString PathStr "/" ) ) )

(defun PathDirName ( PathStr )
  (let (
        ( Components ( PathGetPathComponents PathStr ) ) )
    (let (
          ( RevComponents ( cdr ( reverse Components ) ) ) )
      (let (
            ( NewComponents (if RevComponents
                                ( reverse RevComponents )
                              ( ncons "." )
                              )
                            )
            ( ResultStr nil ) )
        ( foreach 
          Component 
          NewComponents
          (if ResultStr
              ( setq ResultStr ( sprintf nil "%s/%s" ResultStr Component ) )
            ( setq ResultStr Component ) ) )
        ResultStr ) ) ) )

(defun PathBaseName ( PathStr )
  ( car ( last ( PathGetPathComponents PathStr ) ) ) )
  
(defun PathGetPathForFile ( Dir File )
 if( Dir==nil || Dir=="" || PathIsAbsolutePath( File ) || PathIsRelativeToHome( File )
     File
   (if ( equal ( substring Dir ( strlen Dir ) ) "/" )
       ( strcat Dir File )
     ( strcat Dir ( strcat "/" File ) ) ) ) )

(defun PathMakePathAbsolute ( PathStr )
  (cond
   (
    ( PathIsAbsolutePath PathStr )
    PathStr )
   (
    ( PathIsRelativeToHome PathStr )
    (let (
          ( PathWithoutTilda ( substring PathStr 2 ) ) )
      (if PathWithoutTilda
          ( strcat
            ( getShellEnvVar "HOME" )
            PathWithoutTilda )
        ( getShellEnvVar "HOME" ) ) ) )
   (
    t
    ( strcat
      ( PathMakePathAbsolute
        ( getWorkingDir ) )
      "/"
      PathStr ) ) ) )
