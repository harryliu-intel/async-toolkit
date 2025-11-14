; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$

(defun PackageGetPackageRoot ()
  "$packageroot$" )

(defun PackageGetBinRoot () 
  getShellEnvVar("VIRTUOSO_BIN") ) 

(defun PackageGetBinRootForArch ( ArchString )
  ( sprintf nil "%s/%s/bin" ( PackageGetPackageRoot ) ArchString ) )

(defun LoadProjSW ( filename )
  (load (strcat VirtuosoHome "/share/skill/proj/" filename) ) )
