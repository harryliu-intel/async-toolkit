MODULE Main ;

IMPORT Spec ;
IMPORT Node ;
IMPORT IO ;
IMPORT Fmt ;

BEGIN
VAR root := NEW( REF Node.T ) ;
VAR current_node := NEW( REF Node.T ) ;

BEGIN

root^.val := "Module.Rule1" ;
root^.cat := Node.Category.NonTerminal ;
root^.children := NEW( REF Node.DList ) ;

current_node^.val := "MODULE" ;
current_node^.cat := Node.Category.Constant ;
current_node^.children := NEW( REF Node.DList ) ;

Node.AppendNode( root^.children , current_node ) ;

current_node^.val := "Id.Rule1" ;
current_node^.cat := Node.Category.NonTerminal ;
current_node^.children := NEW( REF Node.DList ) ;

Node.AppendNode( current_node^.children , NEW( REF Node.T , val := "Main" , cat := Node.Category.Identifier , children := NEW( REF Node.DList ) ) ) ;
Node.AppendNode( root^.children , current_node ) ;
Node.AppendNode( root^.children , NEW( REF Node.T , val := ";" , cat := Node.Category.Constant , children := NEW( REF Node.DList ) ) ) ;
IO.Put( "Root node length should be 3: " & Fmt.Int( Node.Length( root^.children ) ) & "\n" ) ;
IO.Put( "Id.Rule1 node length should be 1: " & Fmt.Int( Node.Length( current_node^.children ) ) & "\n" ) ;

TRY
	Spec.DebugTree( root , "./test" ) ;
EXCEPT
	| Spec.InvalidFname => IO.Put( "Can't use that fname.\n" ) ;
	| Spec.OutError => IO.Put( "Outerror.\n" ) ;
END ;
END ;

END Main .
