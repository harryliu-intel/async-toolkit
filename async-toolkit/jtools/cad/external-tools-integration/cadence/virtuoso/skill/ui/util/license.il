; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun LicenseGetLicense ( LicenseName Version NumLicensesToLeave 
                           @key (LicenseType "WAIT") 
                                (LicenseCount 1)
                                (Verbose t) )
  (while
      ( lessp
        ( LicenseGetNumAvailable LicenseName )
        NumLicensesToLeave )
    (if Verbose ( printf "Waiting for %s license...\n" LicenseName ) )
    ( ipcSleep 10 ) )
  (let (
        ( Status
          ( lmCheckOut 
            LicenseName
            Version
            LicenseCount
            LicenseType ) ) )
    (if Verbose
        (if Status
            ( printf "Got %s license\n" LicenseName )
          ( printf "Failed to get %s license\n" LicenseName ) ) )
    Status )
)

(defun LicenseGetNumAvailable ( LicenseName ) 
  (let (
        ( ServerList 
          ( lmUserList LicenseName  ) ) )
    (let (
          ( UniqueServers ( ListUniq ( sort ServerList
                                            (lambda ( X Y ) ( lessp ( cadr X ) ( cadr Y ) ) ) ) ) ) )
      ( ListFindSum
        UniqueServers
        (lambda ( Server )
          ( difference
            ( nth 1 Server )
            ListFindSum( nth(2 Server) 
              lambda((Machine) nth(3 Machine) ) )
         ) ) ) ) ) )

(defun LicenseReturnLicense ( LicenseName )
  ( lmCheckIn LicenseName ) )
 
