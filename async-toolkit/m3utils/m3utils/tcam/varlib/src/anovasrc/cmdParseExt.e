%source cmd.t cmd.y
%import cmdLexExt cmdParse

%module { 
IMPORT Process;
IMPORT LongrealList;
IMPORT Text, Fmt;
IMPORT drdist;
IMPORT Math;
}

%interface { 
IMPORT TextLongRealTbl;
IMPORT LongrealList;
}

%public {
	val : LONGREAL;
	symtab : TextLongRealTbl.T;
}

start:
  expr 		{ self.val := $1.val }

expr: 		{ val : LONGREAL }
  literal       {
		  IF NOT self.symtab.get($1.val, $$.val) THEN
		     Process.Crash("Unknown variable \"" & $1.val & "\"")
                  END
    		}
  constant	{ $$ := $1.val }
  sum		{ $$ := $1 + $2 }
  diff		{ $$ := $1 - $2 }
  prod		{ $$ := $1 * $2 }
  quot		{ $$ := $1 / $2 }
  uminus        { $$ := -($1) }
  paren		{ $$ := $1 }
  func          {
 		  PROCEDURE Args(num : CARDINAL) =
		    VAR
		      len := LongrealList.Length($2.val);
		    BEGIN
		      IF len # num THEN
		        Process.Crash("Wrong # of arguments for \"" &
				$1.val & " " & Fmt.Int(len) & " should be "&
				Fmt.Int(num))
		      END
		    END Args;

                  BEGIN  
		    IF Text.Equal($1.val,"log") THEN
		      Args(1);
		      $$ := Math.log($2.val.head)
		    ELSIF Text.Equal($1.val, "exp") THEN
		      Args(1);
		      $$ := Math.exp($2.val.head)
		    ELSIF Text.Equal($1.val, "sqrt") THEN
		      Args(1);
		      $$ := Math.sqrt($2.val.head)
		    ELSIF Text.Equal($1.val, "log10") THEN
		      Args(1);
		      $$ := Math.log10($2.val.head)
		    ELSIF Text.Equal($1.val, "tan") THEN
		      Args(1);
		      $$ := Math.tan($2.val.head)
		    ELSIF Text.Equal($1.val, "sin") THEN
		      Args(1);
		      $$ := Math.sin($2.val.head)
		    ELSIF Text.Equal($1.val, "cos") THEN
		      Args(1);
		      $$ := Math.cos($2.val.head)
		    ELSIF Text.Equal($1.val, "atan") THEN
		      Args(1);
		      $$ := Math.atan($2.val.head)
		    ELSIF Text.Equal($1.val, "tanh") THEN
		      Args(1);
		      $$ := Math.tanh($2.val.head)
		    ELSIF Text.Equal($1.val, "atanh") THEN
		      Args(1);
		      $$ := Math.atanh($2.val.head)
		    ELSIF Text.Equal($1.val, "abs") THEN
		      Args(1);
		      $$ := ABS($2.val.head)
		    ELSIF Text.Equal($1.val, "floor") THEN
		      Args(1);
		      $$ := FLOAT(FLOOR($2.val.head),LONGREAL)
		    ELSIF Text.Equal($1.val, "ceiling") THEN
		      Args(1);
		      $$ := FLOAT(CEILING($2.val.head),LONGREAL)
		    ELSIF Text.Equal($1.val, "trunc") THEN
		      Args(1);
		      $$ := FLOAT(TRUNC($2.val.head),LONGREAL)
		    ELSIF Text.Equal($1.val, "round") THEN
		      Args(1);
		      $$ := FLOAT(ROUND($2.val.head),LONGREAL)
		    ELSIF Text.Equal($1.val, "kronecker") THEN
		      Args(1);
                      IF ROUND($2.val.head) = 0 THEN 
                        $$ := 1.0d0
                      ELSE
                        $$ := 0.0d0
                      END
		    ELSIF Text.Equal($1.val, "sign") THEN
		      Args(1);
                      IF    $2.val.head < 0.0d0 THEN
  	                $$ := -1.0d0
                      ELSIF $2.val.head > 0.0d0 THEN
                        $$ := 1.0d0
                      ELSIF $2.val.head = 0.0d0 THEN
                        $$ := 0.0d0
                      ELSE (* NaN *)
                        $$ := $2.val.head
                      END
		    ELSIF Text.Equal($1.val, "studin") THEN
		      Args(2);
		      VAR
			df := ROUND($2.val.head);
			p := $2.val.tail.head;
		      BEGIN
	  	        $$ := drdist.Studin(df,p)
                      END
		    ELSIF Text.Equal($1.val, "pow") THEN
		      Args(2);
		      $$ := Math.pow($2.val.head, $2.val.tail.head)
		    ELSIF Text.Equal($1.val, "min") THEN
		      Args(2);
		      $$ := MIN($2.val.head, $2.val.tail.head)
		    ELSIF Text.Equal($1.val, "max") THEN
		      Args(2);
		      $$ := MAX($2.val.head, $2.val.tail.head)
		    ELSE 
		      Process.Crash("Unknown function \"" & $1.val & "\"")
             	    END
 		  END
		}

expr_list: 	{ val : LongrealList.T }
  single     	{ $$ := LongrealList.List1($1) }
  mult		{ $$ := LongrealList.Cons($1,$2) }
