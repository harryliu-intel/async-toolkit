; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

(defun FlushDesignFiles (cv)
 (let (tempdir cmd cell)
   cell = cv->cellName
   tempdir = ( ConfigFileGetValue TheCDSConfigTable "TEMP" )

   cmd = (strcat "rm -f ./" cell "*")
   (shell cmd)
   cmd = (strcat "rm -rf " tempdir "/*")
   (shell cmd)
 )
)

(defun FlushAndSync (cv)
  (FlushDesignFiles cv)
  (nrSyncToNetlist cv)
)
