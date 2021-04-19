(* $Id$ *)

INTERFACE HTMLOutput;
IMPORT HTMLPage, HTML;

PROCEDURE SetFooter(footer : HTML.Stuff);
PROCEDURE Ship(page : HTMLPage.T);

END HTMLOutput.
