; Profiling macro, usage: (Profile "file.prof" (Function ...))
; Fail immediately if a SKILL developement license cannot be acquired
(defmacro Profile (file @key (licenseName "skillDev")
                             (licenseType "NOWAIT")
                             (profileField ''time)
                        @rest body)
  `(if (LicenseGetLicense ,licenseName (getLicVersion) 0
                          ?LicenseType ,licenseType
                          ?Verbose nil)
       (let (ret errSet)
         (profileReset)
         (profile ,profileField)
         ret=(errset ,@body)     ; trap error so license can be released
         errSet=errset.errset
         (profileSummary ?children t ?file ,file)
         (LicenseReturnLicense ,licenseName)

         ; return the value from evaluating body, rethrow error if necessary
         (if ret
             (car ret)
           (let ((msg (car (nth 4 errSet))))
             (if msg
                 (error (substring msg 9)) ; strip off initial "*Error* "
               (error "%L" errSet)))
         )
       )
     (error "Cannot acquire %s license; try again later" ,licenseName)
   )
)
