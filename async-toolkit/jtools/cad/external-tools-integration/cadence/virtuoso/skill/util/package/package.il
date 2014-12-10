; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$

(defun PackageGetPackageRoot ()
  "$packageroot$" )

(defun PackageGetBinRoot () 
  getShellEnvVar("VIRTUOSO_BIN") ) 

(defun PackageGetBinRootForArch ( ArchString )
  ( sprintf nil "%s/%s/bin" ( PackageGetPackageRoot ) ArchString ) )
