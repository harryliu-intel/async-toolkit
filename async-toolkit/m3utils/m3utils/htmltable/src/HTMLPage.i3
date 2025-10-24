(* $Id$ *)

INTERFACE HTMLPage;
IMPORT HTML;

TYPE
  T <: Public;

  Public = HTML.T OBJECT
  METHODS
    setHead(headerText : TEXT) : T;
    setBody(body : HTML.Stuff) : T;
    setFooter(body : HTML.Stuff);
    addToBody(stuff : HTML.Stuff);
    setBgColor(color : TEXT); (* def. #ffffff *)
    addScript(script : TEXT);
    addBodyTag(tagDef : TEXT);
    addJavaScriptRefresh(milliseconds : CARDINAL);

    renderHeader() : TEXT;
    (* can override this to add stuff to header *)
  END;

  (* this is a bit hacky... *)

  FrameSet <: PubFrameSet;

  Direction = { Rows, Columns };
  (* hmm, don't do 2-D frames yet. *)

  PubFrameSet = T OBJECT
  METHODS
    init(direction : Direction; 
         elements : CARDINAL; 
         widths : REF ARRAY OF LONGREAL := NIL) : FrameSet;
    (* direction is if frameset should go by rows or columns,
       elements says how many parts it should consist of,
       width is the allocation of widths/lengths, in arbitrary units. *)

    addElement(idx : CARDINAL; element : Frame);

    (* NOT PERMITTED:
       setFooter
       setBody
       addToBody
       setBgColor
       addBodyTag
       addScript *)
  END;

  Frame = BRANDED Brand & " Frame" OBJECT END;
  
  Nested = Frame OBJECT set : FrameSet END;

  URL = Frame OBJECT url : TEXT END;

CONST Brand = "HTMLPage";

END HTMLPage.
