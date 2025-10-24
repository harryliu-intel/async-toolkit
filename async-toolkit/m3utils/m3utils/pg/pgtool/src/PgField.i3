INTERFACE PgField;

TYPE T = { Name, Base, Length, Group };

CONST Names = ARRAY T OF TEXT { "name", "base", "length", "policy_group" };
      
CONST Brand = "PgField";

END PgField.
