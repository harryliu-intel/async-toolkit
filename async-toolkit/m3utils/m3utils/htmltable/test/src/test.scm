(define mysql (new-modula-object '(Database . MySQL)))

(modula-type-op 'Database.MySQL 'call-method mysql 'init)

(Database.SetStatic mysql)

(Database.OpenRemote "localhost" '() "test_database" "testuser" "password")

(Database.Exec "select * from test1;" #f)
