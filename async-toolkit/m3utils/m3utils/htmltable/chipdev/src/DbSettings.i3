INTERFACE DbSettings;
IMPORT Database;

CONST Hostname = "localhost";
      Port     = NIL; (* default value will be chosen *)
      User     = "testuser";
      Passwd   = "password";
      Name     = "hw_rrc";
      Type     = Database.Type.MySQL;

CONST Brand = "DbSettings";

END DbSettings.
