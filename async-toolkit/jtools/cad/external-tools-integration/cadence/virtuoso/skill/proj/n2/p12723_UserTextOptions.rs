// $Header: /ds/data/2647/server_vault/Projects/drwork/x12dev_drwork/rst/x12dev_drwork_rst/PXL/p12723_UserTextOptions.rs.rca 1.8 Wed Oct 22 11:37:31 2014 sstalukd Experimental $

// $Log: p12723_UserTextOptions.rs.rca $
// 
//  Revision: 1.8 Wed Oct 22 11:37:31 2014 sstalukd
//  For cadnav, also replace '//' with '.', similar to RCextract, based on request from Shawn McPeek
// 
//  Revision: 1.7 Tue Aug 12 16:30:13 2014 jhannouc
//  Added support for new text handling in HV flow when DR_HV_ONLY_LIST_TEXT is set to YES. HSD#2551.
// 
//  Revision: 1.6 Tue Jan  7 09:14:18 2014 cpark3
//  updated RC extraction section per DTS
// 
//  Revision: 1.5 Tue Nov  5 13:11:03 2013 kperrey
//  hsd 1929 ; only replace f-slash with period when _drRCextract
// 
//  Revision: 1.4 Mon Apr  2 08:21:13 2012 kperrey
//  fix #if defined syntax missing closing paren
// 
//  Revision: 1.3 Tue Mar  6 12:28:21 2012 kperrey
//  as per rcteam handle synth nets as they want
// 
//  Revision: 1.2 Wed Aug 31 17:10:38 2011 kperrey
//  add if (_drCaseSensitive == _drNO) to control if whether to be casesensitive or not - default is insensitive
// 
//  Revision: 1.1 Mon Oct  4 20:07:00 2010 kperrey
//  pull replace_text and replace_text_characters from p12723_options.rs
// 
//

#ifndef _P12723_USERTEXTOPTIONS_RS_
#define _P12723_USERTEXTOPTIONS_RS_

     // To support new HV naming and propagation convention only for outside customers
     // This will ignore any net name not found in the HV flow voltage lists.
     // Here we are deleting the text (similar to replace with empty string)
     #if (_drTRCVIEW == _drHV && _drHV_ONLY_LIST_TEXT == _drYES)
     	delete_text = { { cells={"*"}, text = no_HV_no_NOM_list  } },
     #endif

     delete_text = { { cells={ "d04hlv*", "d04hlh*", "d04hlc*", "d04hli*", "d04spc*" }, text = { "*" } } },

     // This replaces text string with the replacement text_string
     replace_text = { 
        #if _drTRCVIEW == _dralternate || _drTRCVIEW == _drstandard
           #if (_drCaseSensitive == _drNO) // UPPERCASE
              // remove float* text - simplifies lvs and black box
              {search_strings = "FLOATISS*", replace_string = "" },
           #else
              // remove float* text - simplifies lvs and black box
              {search_strings = "floatiss*", replace_string = "" },
           #endif  // case
        #endif // trcview
        // remove text that have =(equal) in the name 
        #if (defined(_drRCextract))
            #if (defined(_drRenameAllSyn))
                {search_strings = {"*=*syn*", "*=*SYN*"}, replace_string = "" }
            #else
                {search_strings = {"*=*syn*", "*=*SYN*"}, replace_string = "", cells = UserNoExplodeList }
            #endif  // ifndef _drRenameAllSyn
			#if (!defined(_drRetainCCPwr))
				,
					{search_strings = {"VCC*", "vcc*"}, replace_string = "", cells = UserNoExplodeList },
					{search_strings = {"VSS", "vss"}, replace_string = "", cells = UserNoExplodeList }
			#endif
        #else
           // remove text that have =(equal) in the name 
           {search_strings = "*=*", replace_string = "" }
        #endif  // _drRCextract

     } ,  // end replace_text

     // This replaces single text chars with the replacement text_string
     replace_text_characters = {

        #if  (defined(_drRCextract) || defined(_drCADNAV))
           // replace /(forwardSlash) with .(dot)
           {search_string = "/", replace_string = "." } ,
           // remove text that have =(equal) in the name
           {search_string = "=", replace_string = "" }
        #endif  // _drRCextract

     },  // end replace_text_characters

#endif
