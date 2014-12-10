procedure( ShowNameInstance( )

	sh( "rm -f temp.bk" )
	sh( "echo \"leHiEditDisplayOptions()\" > temp.bk" )
	sh( "echo \"leDisplayOptionsForm->instName->value = \\\"instance\\\"\" >> temp.bk" )
	sh( "echo \"hiFormDone(leDisplayOptionsForm)\" >> temp.bk" )
	hiReplayFile( "temp.bk" )

); end procedure

procedure( ShowNameMaster( )

	sh( "rm -f temp.bk" )
	sh( "echo \"leHiEditDisplayOptions()\" > temp.bk" )
	sh( "echo \"leDisplayOptionsForm->instName->value = \\\"master\\\"\" >> temp.bk" )
	sh( "echo \"hiFormDone(leDisplayOptionsForm)\" >> temp.bk" )
	hiReplayFile( "temp.bk" )

); end procedure



(defun InitNameToggle ()
  ShowNameToggle=0
)
(InitNameToggle)


(defun ToggleNameDisplay ()
 (let (tempdir filename file)
  tempdir = ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
  filename = (sprintf nil "%s/showname.replay" tempdir)
  file = (outfile filename)

  ShowNameToggle=ShowNameToggle+1

  (fprintf file "leHiEditDisplayOptions()\n")
  (if (mod ShowNameToggle 2)==0 then
    (fprintf file "leDisplayOptionsForm->instName->value = \"instance\"\n")
  else
    (fprintf file "leDisplayOptionsForm->instName->value = \"master\"\n")
  )
  (fprintf file "hiFormDone(leDisplayOptionsForm)\n")

  (close file)

  (hiReplayFile filename)
 )
)

