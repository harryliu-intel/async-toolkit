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
