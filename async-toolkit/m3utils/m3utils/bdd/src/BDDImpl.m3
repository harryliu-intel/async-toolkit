(* $Id$ *)
(* revelation of BDD.T *)
MODULE BDDImpl EXPORTS BDD, BDDPrivate;
IMPORT BDDPair;
IMPORT BDDTripleHash;
IMPORT Word;

IMPORT Fmt;

TYPE
  Op = { And, Not };
  Pair = BDDPair.T;

REVEAL
  T = BRANDED Brand OBJECT
    l , r : T;
    root : Root;
    tag : CARDINAL; (* for hashing *)
  METHODS
    init() : T := Init;
  END;

(* this special object is used as a literal. *)
(* in order to allow garbage collection, the caches and the lookup table *)
(* are stored in this object instead of as static data structures *)
(* N.B. for a multithreaded application, this structure will need to be *)
(* monitored. *)
TYPE
  Root = T OBJECT
    mu : MUTEX; (* as yet unused *)
    id : CARDINAL;
    tab : BDDTripleHash.T;
    cache : ARRAY Op OF BDDTripleHash.T;
  END;

VAR
  mu := NEW(MUTEX);
  nextTag := 0;
  
PROCEDURE Init(self : T) : T = 
  BEGIN LOCK mu DO self.tag := nextTag; INC(nextTag) END; RETURN self END Init;

PROCEDURE GetId(self : T) : CARDINAL = BEGIN RETURN self.root.id END GetId;

PROCEDURE Order(VAR b1, b2 : T) = 
  BEGIN
    IF b1.root.id > b2.root.id THEN VAR x := b1; BEGIN b1 := b2; b2 := x END END
  END Order;
    
(* hmm *)

PROCEDURE And(b1, b2 : T) : T =
  VAR 
    tripleHash : BDDTripleHash.T;
    l, r : T; 
    b : T;
  BEGIN
    IF b1 = b2 THEN RETURN b1
    ELSIF b1 = false OR b2 = false THEN RETURN false
    ELSIF b2 = true THEN RETURN b1
    ELSIF b1 = true THEN RETURN b2
    END;

    Order(b1,b2);

    tripleHash := b1.root.cache[Op.And];

    IF BDDTripleHash.Get(tripleHash, Pair { b1, b2 } , b) THEN
      RETURN b
    END;
    
    IF b1.root.id = b2.root.id THEN
      l := And(b1.l, b2.l);
      r := And(b1.r, b2.r)
    ELSE
      l := And(b1.l, b2);
      r := And(b1.r, b2)
    END;

    IF l = r THEN
      EVAL BDDTripleHash.Put(tripleHash, Pair { b1, b2 }, (l));
      RETURN l
    END;

    IF NOT BDDTripleHash.Get(b1.root.tab, Pair { l, r } , b) THEN
      b := NEW(T).init();
      b.root := b1.root;
      b.root.id := b1.root.id;
      b.l := l;
      b.r := r;
      EVAL BDDTripleHash.Put(b.root.tab, Pair { l, r }, (b));
    END;

    EVAL BDDTripleHash.Put(tripleHash, Pair { b1, b2 } , (b));
    RETURN b

  END And;

PROCEDURE Not(b1 : T) : T = 
  VAR 
    tripleHash : BDDTripleHash.T;
    b, l, r : T;
  BEGIN
    IF b1 = true THEN RETURN false
    ELSIF b1 = false THEN RETURN true
    END;

    tripleHash := b1.root.cache[Op.Not];
    IF BDDTripleHash.Get(tripleHash, Pair { b1, true }, b) THEN
      RETURN b
    END;

    l := Not(b1.l);
    r := Not(b1.r);
    
    IF NOT BDDTripleHash.Get(b1.root.tab, Pair { l, r }, b) THEN
      b := NEW(T).init();
      b.root := b1.root;
      b.l := l;
      b.r := r;
      EVAL BDDTripleHash.Put(b1.root.tab, Pair { l, r }, (b))
    END;

    EVAL BDDTripleHash.Put(tripleHash, Pair { b1, true }, (b));
    RETURN b

  END Not;

PROCEDURE True() : T = BEGIN RETURN true END True;

PROCEDURE False() : T = BEGIN RETURN false END False;

PROCEDURE New() : T = 
  VAR res : Root := NEW(Root).init(); BEGIN 
    res.root := res;
    res.l := true; res.r := false; 
    res.id := nextId;
    res.tab := NEW(BDDTripleHash.Default).init(128);
    res.cache :=  ARRAY Op OF BDDTripleHash.T { NEW(BDDTripleHash.Default).init(64),
                                                NEW(BDDTripleHash.Default).init(64) };

    EVAL BDDTripleHash.Put(res.tab, Pair { res.l, res.r }, (res));
    INC(nextId);
    RETURN res
  END New;

PROCEDURE Format(x : T) : TEXT =
  BEGIN
    IF x = true THEN RETURN "TRUE"
    ELSIF x = false THEN RETURN "FALSE"
    END;

    RETURN Fmt.Int(x.root.id) & " && (" & Format(x.l) & ") || (" & Format(x.r) &
           ") && ~" & Fmt.Int(x.root.id)
  END Format;
  
<*INLINE*>PROCEDURE Hash(a : T) : Word.T = 
  BEGIN RETURN a.tag END Hash;

VAR
  true,false : Root;
  nextId : CARDINAL := 2;

BEGIN 
  true := NEW(Root).init();
  true.root := true;
  true.id := 1;
  true.r := true;
  true.l := true;

  false := NEW(Root).init();
  false.root := false;
  false.r := false;
  false.l := false;
  false.id := 0;
END BDDImpl.









