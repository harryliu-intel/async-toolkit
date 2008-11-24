;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Code for auto-generating synchronized SQL defn's and Modula-3 code
;;
;; Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.
;;
;; Author: Mika Nystrom <mika@alum.mit.edu>
;;
;;
;; A database consists of tables
;; tables consist of fields
;; e.g.
;;
;;
;; (make-database "db" 
;;  (make-table "table1" 
;;   (make-field "f1" 'data-type1 'data-option1 'data-option2 ...)
;;    ...
;;  )
;; )
;;
;; This code uses and is synchronized with:
;; 1. Database and DatabaseTable Modula-3 interfaces in htmltable lib.
;; 2. templates in mscheme library
;; 3. generic TableMonitor.{im}g
;; 4. scheme-lib/scodegen.scm (that is this file, actually!)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-modules "basic-defs" "display")

;;
;; PUBLIC ROUTINES FOR BUILDING THE INPUTS TO THE BELOW CODE
;;

(define (make-table name . x) (cons name x))

(define (make-field name . x) (cons name x))

(define (make-database name . x) (cons name x))

;;;;;

(define (db-name db) (car db))

(define (get-table-names db) (map car (cdr db)))

(define (get-tables db)      (cdr db))

(define (get-fields tab)     (cdr tab))

;; after setting things up with the above routines, call
;;
;;   write-database-sql 
;;   write-database-m3 
;;
;; as desired.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define port '()) ;; this is an ugly global for now (makes things 
                  ;; a little bit easier to debug...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           INTER-LANGUAGE DATA TYPE DEFINITIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; type conversion from SQL to Modula-3 and back:
;; (sql->m3-proc m3->sql-proc modula-3-function-name modula-3-exceptions)

(define (m3-sym a b) (cons a b))
(define (m3-intf s) (car s))
(define (m3-nam s) (cdr s))

(define int-conversion
  ;; convert integers SQL<->M3
  (list identity
	(m3-sym "Fmt" "Int")
	(m3-sym "Scan" "Int") 
	(list (m3-sym "Lex" "Error")
	      (m3-sym "FloatMode" "Trap"))))

(define bool-conversion
  ;; convert booleans SQL<->M3
  (list identity 
				(m3-sym "Fmt" "Bool")
				(m3-sym "PGSQLScan" "Bool") 
				(list (m3-sym "Lex" " Error"))))

(define str-conversion
  ;; convert strings SQL<->M3
  (list identity 
				(m3-sym "Database" "EscapeQ")
				(m3-sym "Database" "Unescape")
				'()))

(define ts-conversion
  ;; convert timestamps SQL<->M3
  (list (lambda (fieldname) (string-append "extract (\'epoch\' from " 
					   fieldname ")"))
	(m3-sym "Fmt"
		(lambda (x) (string-append "Fmt.LongReal(" 
					    x 
					    ") & \"* interval \'1 second\'\"")))
	(m3-sym "Scan" "LongReal") 
	(list (m3-sym "Lex" "Error")
	      (m3-sym "FloatMode" "Trap"))))

;;
;; NOW LIST ALL THE TYPES WE KNOW ABOUT AND CAN HANDLE...
;;

(define types
  (list
   ;; sym-name, SQL-name, m3-name, m3-must-import, sql-m3-read
   (list 'serial       "serial"         "INTEGER"   #f  int-conversion )
   (list 'varchar      "varchar"        "TEXT"      #f  str-conversion )
   (list 'integer      "integer"        "INTEGER"   #f  int-conversion )
   (list 'boolean      "boolean"        "BOOLEAN"   #f  bool-conversion )
   (list 'timestamp    "timestamp with time zone" 
                                        "Time"      #t  ts-conversion)
   (list 'id           "integer"        "INTEGER"   #f  int-conversion)  
   )
)

(define (find-type type action types)
  ;; extract a type defn from types table
  (define (f lst t)
    (cond ((null? lst) (error "unknown type "  type))
	  ((eq? (caar lst) t) (action (car lst)))
	  (else (f (cdr lst) t))))

  (if (list? type) 
      (f types (car type))
      (f types type)))

;; get various details about a known type...
(define (get-conversion type) (nth type 4))

(define (type->sql-string type) (find-type type cadr types))

(define (type->m3-typename type) 
  (if (m3-import? type)
      (string-append (find-type type caddr types) ".T")
      (find-type type caddr types))
)

(define (type->m3-intfname type) (nth (find-type type identity types) 2))

(define (m3-import? type) (nth (find-type type identity types) 3))
;; does the type require extra M3 imports?

(define (options-string db lst) 
  ;; SQL column options
  (define (stringify-default def)
    (cond ((string? def) def)
	  (else (stringify def))))

  (if (null? lst) 
      ""
      (string-append
       (cond ((list? (car lst))
	      (if (eq? (caar lst) 'default) 
		  (string-append " default " (stringify-default (cadar lst)))))
	     ((eq? (car lst) 'not-null) " not null ")
	     ((eq? (car lst) 'primary-key) " primary key ")
	     (else (error "Unknown options " lst)))
       (options-string db (cdr lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    CODE GENERATION....
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (put-header db name) (dis dnl "create table " name dnl "(" dnl port))
(define (put-trailer db)  (dis ");" dnl dnl port))

(define (put-field-list db field)
  (let ((name (car field))
	(type (cadr field))
	(options (cddr field)))

    (dis "  " name " " (type->sql-string type) " " (options-string db options)
	 port)))

(define (put-field db . x) (put-field-list db x))

(define (make-drop-table-sql tbl db)
  (let ((name (car tbl)))
    (dis "drop table " name " cascade;" dnl port)))

(define (make-write-seq p)
  (dis "create sequence write_seq;" dnl p))

(define (drop-write-seq p)
  (dis "drop sequence write_seq cascade;" dnl p))



(define (make-table-sql tbl db)
  (let ((name (car tbl))
	(fields (get-fields (complete-tbl tbl))))
    
    (put-header db name)
    (map2 (lambda (f) (begin (put-field-list gcoms f)
			     (dis "," dnl port)))

	  (lambda (f) (put-field-list gcoms f))
	  
	  fields)
    (dis dnl port)
    (put-trailer db)
    #t
))

(define (putm3-field-list fld port)
    (let ((name (car fld))
	  (type (cadr fld)))

      (dis "  " name " : " (type->m3-typename type) ";" dnl port)
      (dis "  " name "_isNull : BOOLEAN := TRUE;" dnl port)

))

(define (put-getter-procedure fld ip mp)
  (let ((name (car fld))
				(not-null (memq 'not-null (cddr fld)))
				(m3type (type->m3-typename (cadr fld))))

		;;(dis "put-getter-procedure " let " : " fld dnl dnl '())

    (map
     (lambda(port)
       (dis "PROCEDURE Get_" name "(READONLY t : T) : " m3type (if not-null "" " RAISES { DBTable.IsNull }")
	    port))
     (list ip mp))

    (dis ";" dnl ip)

    (dis " =" dnl 
				 (if not-null (string-append 
	 "  <*FATAL DBTable.IsNull*>" dnl) "" )
	 "  BEGIN" dnl
	 "    IF t." name "_isNull THEN" dnl
         "      RAISE DBTable.IsNull(TableName & \"." name "\")" dnl
	 "    ELSE" dnl
         "      RETURN t. " name dnl
	 "    END" dnl
         "  END Get_" name ";" dnl dnl
	 mp)
))
    
(define (put-setter-procedure fld ip mp)
  (let ((name (car fld))
	(m3type (type->m3-typename (cadr fld))))
    (map
     (lambda(port)
       (dis "PROCEDURE Set_" name "(VAR t : T; val : " m3type ")"
	    port))
     (list ip mp))

    (dis ";" dnl ip)

    (dis " =" dnl 
	 "  BEGIN" dnl
	 "    t." name "_isNull := FALSE;" dnl
   "    t." name " := val" dnl
   "  END Set_" name ";" dnl dnl
	 mp)
))
    
(define (put-clearer-procedure fld ip mp)
  (let ((name (car fld))
				(not-null (memq 'not-null (cddr fld)))
				(m3type (type->m3-typename (cadr fld))))
		(if not-null #f
				(begin (map
								(lambda(port)
									(dis "PROCEDURE Clear_" name "(VAR t : T)"
											 port))
								(list ip mp))
							 
							 (dis ";" dnl ip)
							 
							 (dis " =" dnl 
										"  BEGIN" dnl
										"    t." name "_isNull := TRUE;" dnl
										"  END Clear_" name ";" dnl dnl
										mp)
				))))
    
(define (put-have-procedure fld ip mp)
  (let ((name (car fld))
	(m3type (type->m3-typename (cadr fld))))
    (map
     (lambda(port)
       (dis "PROCEDURE Have_" name "(READONLY t : T; VAR " name " : " m3type ") : BOOLEAN "
	    port))
     (list ip mp))

    (dis ";" dnl ip)

    (dis " =" dnl 
	 "  BEGIN" dnl
	 "    IF t." name "_isNull THEN" dnl
   "      RETURN FALSE" dnl
	 "    ELSE" dnl
   "      " name " := t." name ";" dnl
	 "      RETURN TRUE" dnl
	 "    END" dnl
   "  END Have_" name ";" dnl dnl
	 mp)
))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    VARIOUS MODULA-3 ROUTINES FOLLOW
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; OK, there are some routines marked -old here.  These are no longer
;; used.  The generated code from these routines is more efficient
;; than what we have now, but it doesn't work in the presence of
;; NULLs!  
;;

(define (ass-sql-old fld)
  (let* ((name (car fld))
	 (m3name (string-append "record." name))
	 (type (find-type (cadr fld) identity types))
	 (conv (get-conversion type))
	 (m3->sql (cadr conv)))
    (string-append name "= \"&"
		   (cond 
		    ((procedure? m3->sql) (m3->sql m3name))
		    ((procedure? (cdr m3->sql))
		     ((cdr m3->sql) m3name))
		    (else
		       (string-append (car m3->sql) "." (cdr m3->sql) "("
				      m3name ")")))
		   dnl "      &\"")))
				      
(define (dis-update-m3-old tbl mp)
  (let ((tbl-name (car tbl)) 
	(fields (get-fields (complete-tbl tbl))))
    (dis "  BEGIN " dnl mp)
    (dis "    EVAL db.tExec(\"update " tbl-name " set " mp)
    (map2 (lambda(fld) (dis (ass-sql-old fld) "," mp))
	  (lambda(fld) (dis (ass-sql-old fld) " where " tbl-name "_id=\"&Fmt.Int(record." 
			    tbl-name "_id)&\")" mp))
	  fields)
    (dis ";\")" dnl mp)
    (dis "  END " mp)

    #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ass-sql fld)
  ;; the string corresponding to a SQL assignment (coded in M3)
  (let* ( (name (car fld))
	  (m3name (string-append "record." name))
	  (type-name (cadr fld)) )

    (string-append 
     "    IF NOT " m3name "_isNull THEN" dnl
     "      query.addhi(\"" name "=\"&" (format-sql-from-m3 m3name type-name) ")"
     dnl
     "    END;" dnl
     )))


(define (dis-update-m3 tbl mp)
  (let ((tbl-name (car tbl)) 
	(fields (get-fields (complete-tbl tbl))))
    (dis "  VAR query := NEW(TextSeq.T).init(); BEGIN " dnl mp)
    (map (lambda(fld) (dis (ass-sql fld) mp)) fields) 
    (dis dnl mp)
    (dis "    EVAL db.tExec(\"update " tbl-name " set \" & TextUtils.FormatInfix(query,\",\") & \" where " 
	 tbl-name "_id=\"&Fmt.Int(record." tbl-name "_id)&\";\")" dnl mp)
    (dis "  END " mp)
    #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (format-sql-from-m3 m3name type-name)
  (let* ((type (find-type type-name identity types))
	 (conv (get-conversion type))
	 (m3->sql (cadr conv)))
    (cond
      ;;
      ;; there are several legal ways of representing the 
      ;; conversion.  could either be a procedure or else
      ;; a Pair, representing the m3 interface and unqualified name
      ;; within that interface, of a standard M3 routine.
      ;;
      ;; (see the types table above)
      ;;
      ((procedure? m3->sql)       (m3->sql       m3name))
      ((procedure? (cdr m3->sql)) ((cdr m3->sql) m3name))
      (else
       (string-append (car m3->sql) "." (cdr m3->sql) "("
		      m3name ")")))))

(define (ins-sql fld)
  ;; the string corresponding to a SQL assignment (coded in M3)
  (let* ((name (car fld))
	 (m3name (string-append "record." name))
	 (type-name (cadr fld)))
    (string-append 
     "    IF NOT " m3name "_isNull THEN" dnl
     "      fields.addhi(\" " name "\");" dnl
     "      values.addhi(" (format-sql-from-m3 m3name type-name) ")" dnl
     "    END;" dnl
     )))

(define (dis-insert-m3 tbl mp)
  (let ((tbl-name (car tbl)) 
	(fields (get-fields (complete-tbl tbl))))
    (dis "  VAR fields, values := NEW(TextSeq.T).init();" dnl
         "  BEGIN" dnl mp)

    (map (lambda(fld) (dis (ins-sql fld) mp))
	 fields) 

    (dis dnl mp)
    (dis "    EVAL db.tExec(\"insert into " tbl-name " (\" & TextUtils.FormatInfix(fields,\",\") & \")\"&"
	 dnl mp)
    (dis "                 \" values (\"& TextUtils.FormatInfix(values,\",\")&\")\")" dnl mp)
    (dis "  END " mp)
    #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ass-m3-old fld)
  (let* ((name (car fld))
	 (type (find-type (cadr fld) identity types))
	 (conv (get-conversion type))
	 (m3conv (caddr conv))
	 (query-op (car conv)))
    (string-append "      " 
		   (if (null? m3conv) 
		       ""
		       (string-append (car m3conv) "." (cdr m3conv)))
		   "(row.get(\"" name "\"))")))


(define (ass-q fld)
  (let* ((name (car fld))
	 (type (find-type (cadr fld) identity types))
	 (conv (get-conversion type))
	 (query-op (car conv)))
    (string-append (query-op name) " as " name)))

(define (dis-parse-m3-old tbl mp)
  (let ((tbl-name (car tbl)) 
	(fields (get-fields (complete-tbl tbl))))
    (dis "  BEGIN " dnl mp)
    (dis "    RETURN T { " dnl mp)
    (map2 (lambda (fld) (dis (ass-m3-old fld) "," dnl mp))
	  (lambda (fld) (dis (ass-m3-old fld) "" dnl mp)) 
	  fields)
    
    (dis "    }" dnl mp)
    (dis "  END " mp)

    #t))

(define (ass-m3 fld)
  ;; the string corresponding to an assignment to an M3 var. (starting from
  ;; the output of a SQL query)
  (let* ((name (car fld))
	 (type (find-type (cadr fld) identity types))
	 (conv (get-conversion type))
	 (m3conv (caddr conv))
	 (query-op (car conv)))
    (string-append
     "    res." name "_isNull := row.getIsNull(\"" name "\");" dnl
     "    IF NOT res." name "_isNull THEN" dnl
     "      res." name " := "
     (if (null? m3conv) 
	 ""
	 (string-append (car m3conv) "." (cdr m3conv)))
     "   (row.get(\"" name "\")) " dnl 
     "    END"
)))
     

(define (dis-parse-m3 tbl mp)
  ;; print the entire routine for parsing a DB row into an M3 RECORD
  (let ((tbl-name (car tbl)) 
	(fields (get-fields (complete-tbl tbl))))
    (dis "  VAR res : T; BEGIN " dnl mp)
    (map (lambda (fld) (dis (ass-m3 fld) ";" dnl mp))
	  fields)

    (dis "    RETURN res;" dnl mp)
    (dis "  END " mp)

    #t))



(define (dis-dirty-m3 tbl mp)
  ;; print the entire routine for manipulating the dirty bit
  (let ((tbl-name (car tbl)))
    (dis "  BEGIN " dnl mp)

    (dis "    IF row # AllRows THEN " dnl mp)

    (dis "      EVAL db.tExec(\"update " 
	 tbl-name 
        " set dirty = \" & Fmt.Bool(to) & \" where (\" & restriction & \") and " tbl-name "_id = \" & Fmt.Int(row) & \";\")" 
	 dnl mp)      

    (dis "    ELSE " dnl mp)

    (dis "      EVAL db.tExec(\"update " 
	 tbl-name 
	 " set dirty = \" & Fmt.Bool(to) & \" where \" & restriction & \";\")" 
	 dnl mp)

    (dis "    END " dnl mp)

    (dis "  END " mp)

    #t))
	  

(define (dis-query-m3 tbl mp)
  ;; print the entire routine that generates the 'standard query string'
  (let ((tbl-name (car tbl)) 
	(fields (get-fields (complete-tbl tbl))))
    (dis "  BEGIN " dnl mp)
    (dis "    RETURN \"" mp)
    (map2 (lambda (fld) (dis (ass-q fld) "," mp))
	  (lambda (fld) (dis (ass-q fld) ""  mp)) 
	  fields)
    (dis " \"" dnl mp)
    (dis "  END " mp)
    #t
))

(define (dis-getid-m3 tbl mp)
  ;; having a procedure to get the ID lets us write generic
  ;; M3 code, without having to know all the field names.
  (let (  (tbl-name (car tbl))  )
    (dis "  BEGIN " dnl mp)
    (dis "    RETURN  t." tbl-name "_id" dnl mp)
    (dis "  END " mp)
    #t
))
  

(define (dis-imports imports ip)
  (let ((uniq-imports (uniq eqv? imports)))
      (if (not (null? uniq-imports))
	  (begin 
	    (dis "<*NOWARN*>IMPORT " ip)
	    (map2 (lambda (intf) (dis " " intf "," ip))
		  (lambda (intf) (dis " " intf ";" dnl dnl ip))
		  uniq-imports))
	  )
      )
)

(define (dis-intf-imports fields ip)
  (let ((imports
	 (append (list "DBerr" "Database" "DatabaseTable" 
		       "Scan" "Fmt" "Lex" "FloatMode" "PGSQLScan" "TextSeq"
		       "TextUtils" "DBTable")
		 (map type->m3-intfname 
		      (filter m3-import? (map cadr fields))))))
    (dis-imports imports ip) 
    #t
    ))

(define dis-modu-imports dis-intf-imports)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    PUT IT ALL TOGETHER.  GENERATE ALL CODE FOR A SINGLE TABLE!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (make-table-m3 tbl db m3mp)

  ;; generate all the Modula-3 code for accessing a table

  (let* ((name (car tbl))
	 (dbname (db-name db))
	 (m3name (string-append "DBTable_" dbname "_" name))
	 (ip (open-output-file (string-append m3name ".i3")))
	 (mp (open-output-file (string-append m3name ".m3")))
	 (fields (get-fields (complete-tbl tbl))))

    (dis "derived_interface(\"" m3name "\",VISIBLE)" dnl m3mp)
    (dis "derived_implementation(\"" m3name "\")" dnl m3mp)
    (dis "TableMonitor(\"" m3name "\",\"" m3name "\")" dnl dnl m3mp)
    (dis "Table(\"Int" m3name "\",\"Integer\",\"" m3name "\")" dnl dnl m3mp)

    (dis "(* AUTOMATICALLY GENERATED DO NOT EDIT *)" dnl ip)
    (dis "INTERFACE " m3name ";" dnl dnl ip)

    (dis "(* AUTOMATICALLY GENERATED DO NOT EDIT *)" dnl mp)
    (dis "MODULE " m3name ";" dnl dnl mp)

    (dis-intf-imports fields ip)
    (dis-modu-imports fields mp)

    (dis "TYPE T = RECORD" dnl ip)
    (map (lambda (fld) (putm3-field-list fld ip)) fields)
    (dis "END;" dnl ip)
    (dis dnl ip)

    ;; the various field-wise procedures
		(map (lambda(proc)
					 (map (lambda (fld) (proc fld ip mp)) fields))
				 (list put-getter-procedure
							 put-setter-procedure
							 put-clearer-procedure
							 put-have-procedure))

    (let 
	;;
	;; define some Modula-3 procedures.
	;; Update  a row
	;; Parse   a row
	;; Insert  a row
	;; QueryHeader   provide the data spec for the query to Parse
	;; SetDirty  clear/set dirty bit
	;;
	((upheader
	  (list "Update" 
		"(db : Database.T; READONLY record : T) RAISES { DBerr.Error }"
		dis-update-m3
		))
	 (parseheader 
	  (list "Parse"
		"(row : DatabaseTable.T) : T RAISES { Lex.Error, FloatMode.Trap, DBerr.Error }"
		dis-parse-m3
		))
	 (insert
	  (list "Insert"
		"(db : Database.T; READONLY record : T) RAISES { DBerr.Error }"
		dis-insert-m3
		))
	 (queryheader 
	  (list "QueryHeader" "() : TEXT" dis-query-m3))
	 (getid
	  (list "GetRecordId" "(READONLY t : T) : CARDINAL" dis-getid-m3))
	 (dirtyheader 
	  (list
	   "SetDirty" 
	   "(db : Database.T; to : BOOLEAN := TRUE; row : [AllRows .. LAST(CARDINAL)] := AllRows; restriction : TEXT := \"true\") RAISES { DBerr.Error }"
	   dis-dirty-m3
	   ))
	 
	 )
      (dis dnl "CONST AllRows = -1; " dnl ip)

      (map 
       (lambda(h)
	 (let (  (pname (car h))
		 (proto (cadr h))
		 (dis-code  (caddr h))  )
	   (dis "PROCEDURE " pname proto ";" dnl dnl ip)
	   (dis "PROCEDURE " pname proto "=" dnl mp)
	   (dis-code tbl mp)
	   (dis pname ";" dnl dnl dnl mp)))
       (list upheader parseheader insert queryheader dirtyheader getid)
       )
      )

    (dis "CONST Brand = \"" m3name "\";" dnl dnl ip)
    (dis "CONST TableName = \"" name "\";" dnl dnl ip)
    (dis "END " m3name "." dnl ip)     (dis "BEGIN END " m3name "." dnl mp)

    (close-output-port ip) (close-output-port mp)
    #t
    )
  )
    

(define (complete-tbl tbl)
  ;;
  ;; "complete" the table, adding standard columns
  ;; id col.
  ;; created time
  ;; updated time
  ;; dirty bit (for consistency with in-core cache)
  ;; active bit 
  ;;
  (let ((name (car tbl))
	(fields (get-fields tbl)))
	      
    (cons name
	  (append (list
		   (make-field (string-append name "_id") 'serial 'primary-key)
		   (make-field "created" 'timestamp 'not-null (list 'default "now()"))
		   (make-field "updated" 'timestamp (list 'default "now()"))
		   (make-field "dirty" 'boolean 'not-null (list 'default "true"))
		   (make-field "active" 'boolean 'not-null (list 'default "false")))
		  fields))))

(define (is-id? field) 
  (and (list? (cadr field)) 
       (eq? 'id (caadr field))))

(define (display-constraint tab-name c)
  (let ((ftab-name (cadadr c))
	(col-name (car c)))
    (dis dnl
	 "alter table " tab-name 
	 " add constraint " tab-name "_" ftab-name "_" col-name "_fk"
	 " foreign key (" col-name ") references " ftab-name "(" ftab-name 
	 "_id);" dnl port)))

(define (display-constraints tab)
  (let ((tab-name (car tab)))
    (map (lambda (c) (display-constraint tab-name c)) 
	 (filter is-id? (get-fields tab)))
    #t
))
    
(define (display-indexes tab)
  (let ((tab-name (car tab)))
    (dis 
     "create index " tab-name "_updated_idx on " tab-name "(updated);" dnl
     "create index " tab-name "_dirty_idx on " tab-name "(dirty);" dnl
	 dnl port)))
  
(define (find-constraints db)

  (map display-constraints (get-tables db)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    FINALLY, WRITE ALL THE CODE FOR THE DATABASE...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-database-sql db p)
  (begin 
    (set! port p)
    (map (lambda (tbl) (make-drop-table-sql tbl db)) (get-tables db))
    (drop-write-seq p)
    (dis dnl dnl dnl p)
    (make-write-seq p)
    (map (lambda (tbl) (make-table-sql tbl db)) (get-tables db))
    (map display-indexes (get-tables db))
    (map display-constraints (get-tables db))))


(define (write-database-sql db)
  (let ((p (open-output-file (string-append (car db) ".sql"))))
    (make-database-sql db p)
    (close-output-port p)))

(define (write-database-m3 db)
  (let ((p (open-output-file scodegen-m3makefile)))
    (map (lambda (t)
	   (make-table-m3 t db p)) 
	 (get-tables db))
    (close-output-port p)))
