MODULE HTMLOutput;
IMPORT HTML, HTMLPage;
IMPORT Wr, Stdio, Debug;
IMPORT Thread; (* for exceptions *)

VAR
  out := Stdio.stdout;
  footer : HTML.T := NIL;

PROCEDURE SetFooter(newFooter : HTML.Stuff) =
  BEGIN 
    footer := HTML.Wrap(newFooter);
  END SetFooter;

PROCEDURE Ship(page : HTMLPage.T) =
  <* FATAL Wr.Failure, Thread.Alerted *>
  VAR
    shipText : TEXT;
  BEGIN
    shipText := "Content-type: text/html\n\n";

    shipText := shipText & "<html>";
    Debug.Out("Ship setting footer.");
    IF footer # NIL THEN page.setFooter(footer) END;
    Debug.Out("Ship formatting page.");
    shipText := shipText & page.format();
    shipText := shipText & "</html>\n";

    (* here we need to escape apostrophes etc.. is this the right
       place?? *)
    Debug.Out("Ship putting text.");
    Wr.PutText(out,shipText);
    Wr.Flush(out)

  END Ship;

BEGIN END HTMLOutput.
