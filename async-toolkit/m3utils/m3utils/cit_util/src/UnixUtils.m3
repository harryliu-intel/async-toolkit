UNSAFE MODULE UnixUtils;
IMPORT Pathname;
IMPORT M3toC;
IMPORT Unix;
IMPORT OSError;

VAR
  buf : ARRAY[0..999] OF CHAR;
PROCEDURE SymLink(name1, name2: Pathname.T) RAISES {OSError.E} =
  BEGIN
    IF Unix.symlink(M3toC.TtoS(name1), M3toC.TtoS(name2)) # 0 THEN
      (* already exists? *)

      (*
      IF Unix.readlink (M3toC.TtoS(name2), ADR(buf), 1000) # 0 THEN
        RAISE OSError.E(NIL);
      END;

      Why isn't this working??
      *)
    END;
  END SymLink;

BEGIN
END UnixUtils.
