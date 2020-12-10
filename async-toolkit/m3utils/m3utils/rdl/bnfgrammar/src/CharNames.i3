INTERFACE CharNames;

TYPE T = RECORD c : CHAR; nm : TEXT END;

CONST
  Mappings = ARRAY OF T {
  T { '&', "Amp" },
  T { '*', "Ast" },
  T { '%', "Pct" },
  T { '(', "Lpa" },
  T { ')', "Rpa" },
  T { '=', "Equ" },
  T { '~', "Til" },
  T { '|', "Pip" },
  T { '[', "Lsq" },
  T { ']', "Rsq" },
  T { '{', "Lcu" },
  T { '}', "Rcu" },
  T { '-', "Min" },
  T { '$', "Dol" },
  T { '<', "Lst" },
  T { '>', "Grt" },
  T { ',', "Com" },
  T { '+', "Plu" },
  T { '@', "Ats" },
  T { '^', "Car" },
  T { '?', "Que" },
  T { '!', "Exc" },
  T { '/', "Fsl" },
  T { ';', "Sco" },
  T { ':', "Col" }
  };

CONST Brand = "CharNames";

PROCEDURE Map(c : CHAR; VAR to : TEXT) : BOOLEAN;
  
END CharNames.

  
