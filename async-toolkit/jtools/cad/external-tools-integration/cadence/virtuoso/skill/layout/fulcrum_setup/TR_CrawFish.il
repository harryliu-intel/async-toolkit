; ====================================================================================================
; TR_CrawFish_VersionLogField CallBack Function
; ====================================================================================================
procedure( TR_CrawFish_VersionHistoryField_CB( )

	let(( 	TR_CrawFish_VersionHistoryTextField
		TR_CrawFish_VersionHistoryTextForm
		HistoryText
		)

	HistoryText =
	"\n CrawFish 1.1.1
	 \n -- Released Oct-15-2003 7:30am
	 \n -- Added Support for Assura 3.0 and Assura 2.0
	 \n -- -- VLVS View Name Support
	 \n       User can now choose what the VLVS View Name 
         \n       he wants it to be (Default = \"layout.VLVS\"
	 \n -- Misc Fixes and Cleanup
	 \n -- -- The Virtual Pin Dialog Box now use      
         \n       \"Load File\" and \"Save File\" buttons
         \n       to eliminate any confusion
         \n -- -- The \"NetList Path\" and \"BlackBoxFileName\"
         \n       options are now disable   
	 \n
         \n CrawFish 1.1.0
	 \n -- Released Aug-25-2003 12:30pm
	 \n -- Added Support for Assura 3.0
	 \n -- -- \"lvs.<cellName>.state\" File is used
	 \n       to automatically lanuch LVS
	 \n -- -- VLVS will cause unbound pins on Layout side ONLY
	 \n       since Assura 3.0 flags excess pins as unbounded
         \n       all virtual pins created by VLVS will be unbounded 
	 \n
	 \n CrawFish 1.0.4
	 \n -- Released Apr-22-2003 04:30pm
	 \n -- Added \"DramsComeTrue\" switch to PathFinder Module
	 \n -- Now, you can turn the hiLite into real rectangles!!
	 \n
	 \n CrawFish 1.0.3
	 \n -- Released Feb-06-2003 11:10am
	 \n -- Fixed Bug
	 \n -- Now, Partial Virtual Joined Net should 
	 \n    also work on Top-level pins
	 \n
	 \n CrawFish 1.0.2
	 \n -- Released Jan-27-2003 3:50pm
	 \n -- Fixed VLVS Module (TR_AssuraLVS.il)
	 \n -- \"DrawPinsLabels\" is now capable of auto set everything
	 \n    in the LSW window to visible and selectable
	 \n    to bring up pins
	 \n -- Once script is done, it will automatically
	 \n    restore the original LSW
	 \n 
	 \n CrawFish 1.0.1
	 \n -- Released Jan-27-2003 
	 \n -- Fixed VLVS Module
	 \n -- Added support for \"path\" and \"polygon\"
	 \n -- Program now draw a 0.02 by 0.02 um square
	 \n    pins on top of these object types
	 \n 
	 \n CrawFish 1.0
	 \n -- Released Jan-23-2003 (First Release)
	 \n -- Added \"viaSpacing\" Option to PathFinder Module
	 \n -- Added Version History Support
	 \n -- Added \"ClearHiLites\" Button
	 \n 
	 \n
	 \n CrawFish Family (Module List)
	 \n =============================
	 \n Hand Routing and Advanced Verification Tool
	 \n 1. PathFinder
	 \n 2. NetLabeller (done but not released)
	 \n 3. VLVS (Virtual-Blackbox)"


	TR_CrawFish_VersionHistoryTextField = hiCreateMLTextField( 
			?name 		`TR_CrawFish_VersionHistoryTextField
			?defValue 	HistoryText
			?editable	nil
			?enabled 	t
			)

        TR_CrawFish_VersionHistoryTextForm = hiCreateAppForm( 
                ?name 		'TR_CrawFish_VersionHistoryTextForm
		?initialSize  list( 500 500 )
                ?fields  	list(
					list( TR_CrawFish_VersionHistoryTextField 0:0 500:400 100 )
					)
        )

	hiDisplayForm( TR_CrawFish_VersionHistoryTextForm )

	t

	); end let

); end procedure
