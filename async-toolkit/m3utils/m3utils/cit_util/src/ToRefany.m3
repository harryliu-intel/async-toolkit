MODULE ToRefany;
IMPORT ToRefanyClass;
IMPORT ToRefanyTbl;
IMPORT Word;

VAR 
  tbl := NEW(ToRefanyTbl.Default).init();

PROCEDURE AddType(type : ToRefanyClass.T) = 
  VAR
    x : BOOLEAN;
  BEGIN
    x := tbl.put(type.typecode, type);
    <* ASSERT NOT x *>
  END AddType;

PROCEDURE Hash(a : T) : Word.T =
  BEGIN RETURN FindType(a).hash(a) END Hash;

PROCEDURE Equal(a, b : T) : BOOLEAN  =
  BEGIN 
    IF TYPECODE(a) # TYPECODE(b) THEN RETURN FALSE END;

    RETURN FindType(a).equal(a,b) 
  END Equal;

PROCEDURE FindType(a : T) : ToRefanyClass.T =
  VAR
    x : BOOLEAN;
    type : ToRefanyClass.T;
  BEGIN
    x := tbl.get(TYPECODE(a),type);
    <* ASSERT x *>
    RETURN type
  END FindType;

BEGIN END ToRefany.
