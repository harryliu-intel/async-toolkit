list( "3.Clear VUE Highlights" "clearHighlights" )

( UICreateMenuItemCallBack
  `clearHighlightsCB
  (lambda ()
  snpsHvLayRemoveHilites()    
) )
