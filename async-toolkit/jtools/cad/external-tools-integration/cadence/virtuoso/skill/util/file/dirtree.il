; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun DirTreeMakeStackFrame ( Directory )
  (let (
        ( Entries ( getDirFiles Directory ) ) )
    (let (
          ( Files ( ListApplyFuncToListAndAccumulateNonNilResults
                    Entries
                    (lambda
                      ( Entry Directory )
                      (let (
                            ( EntryFullName ( sprintf nil "%s/%s" Directory Entry ) ) )
                        (when ( isFile EntryFullName )
                          EntryFullName ) ) )
                    ( list Directory ) ) )
          ( Dirs ( ListApplyFuncToListAndAccumulateNonNilResults
                   Entries
                   (lambda
                     ( Entry Directory )
                     (unless ( or ( equal Entry "." ) ( equal Entry ".." ) )
                       (let (
                             ( EntryFullName ( sprintf nil "%s/%s" Directory Entry ) ) )
                         (when ( isDir EntryFullName )
                           EntryFullName ) ) ) )
                   ( list Directory ) ) ) )
      ( list
        Directory
        Files
        Dirs ) ) ) )
