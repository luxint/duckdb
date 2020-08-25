;; @module duck.lsp
;; @description database interface modules
;; @version 1.0 initial version 05 august 2020
;;
;; <h2>DuckDB database bindings for newlisp</h2>
;;
;; <h3> "DuckDB is an embeddable SQL OLAP database management system" </h3>
;;
;; Look at https://duckdb.org for detailed desription and documentation.
;;
;; To use this module include the following 'load' statement at the
;; beginning of the program file:
;; <pre>
;; (load "<your directory>/duck.lsp")
;; </pre>
;; Test the module:
;; <pre>
;; (duck:test)
;; </pre>
;;

(context 'duck)

;; You need the DuckDB C dynamic library.
;; See compilation instructions  on https://duckdb.org/docs/installation/ for your OS
;; These bindings have been tested against version compiled on 23 aug 2020, however because
;; this is a pre 1.0 release, anything may break.  
;; Looks for the dynamic library in /home/duckdb/ change as desired
;;
(set 'home (env "HOME"))
(set 'files (list 
    (string  home  "/duckdb/libduckdb.so") ; Linux
    (string home "/duckdb/libduckdb.dylib") ; Mac OSX
    (string home "/duckdb/libduckdb.dll") ; Windows ?
   )) 

(set 'lib (files (or
		       (find true (map file? files)) 
		       (throw-error "cannot find duckdb library"))))

(constant 'DUCKDB_TYPES '(
		DUCKDB_TYPE_INVALID   ;0 
		DUCKDB_TYPE_BOOLEAN   ;1
		DUCKDB_TYPE_TINYINT   ;2
		DUCKDB_TYPE_SMALLINT  ;3
		DUCKDB_TYPE_INTEGER   ;4
		DUCKDB_TYPE_BIGINT    ;5
		DUCKDB_TYPE_FLOAT     ;6
		DUCKDB_TYPE_DOUBLE    ;7
		DUCKDB_TYPE_TIMESTAMP ;8
		DUCKDB_TYPE_DATE      ;9
		DUCKDB_TYPE_TIME      ;10
        DUCKDB_TYPE_INTERVAL, ;11
        DUCKDB_TYPE_HUGEINT ; 12
		DUCKDB_TYPE_VARCHAR )); 13

; globally used vars and constants

(define db nil)                                    ; database handle
(define dbp "\000\000\000\000\000\000\000\000")    ; pointer to database handle
(define con nil)                                   ; connection handle
(define dbc "\000\000\000\000\000\000\000\000")    ; pointer to connection handle
(define result "\000\000\000\000\000\000\000\000") ; pointer to result

(constant 'DuckDBSuccess  0 'DuckDBError 1) ; duckdb_state;

; Opens a database file at the given path (nullptr for in-memory). Returns DuckDBSuccess on success, or DuckDBError on
; failure. [OUT: database]
; duckdb_state duckdb_open(const char *path, duckdb_database *out_database);
(import lib "duckdb_open" "cdecl")

;; @syntax (duck:open [<str-db-name>])
;; @param <str-db-name> The name of the database.
;; @return a list with (<result> <message>)
;; if result true then succesfull
;; Opens or creates a database and a connection. If the database does exist it gets opened, 
;; else a new database with the name given is created.
;; If no database is given an in memory database is created.
;; If trying to open a database that already has been opened 
;; then the list (nil "a database is already open") is returned
;; If there is already a connection ti the database then
;; the list (nil "already connected to database") is returned

(define (duck:open db-path)
	(if (not db)
		(begin
		  (set 'state (duckdb_open db-path dbp))
		  (when (!= state DuckDBSuccess)
				(set 'db nil)
			  (throw-error '(nil "error opening database!")))
			(set 'db (get-long dbp))
			(connect))
		(throw-error '(nil "a database is already open")))
	'(true "OK"))

; Closes the database.
;void duckdb_close(duckdb_database *database);
(import lib "duckdb_close" "cdecl")

;; @syntax (duck:close)
;; Closes the currently open database and connection

(define (duck:close)
	(disconnect)
	(set 'db nil)
	(duckdb_close dbp))

; Creates a connection to the specified database. [OUT: connection]
; duckdb_state duckdb_connect(duckdb_database database, duckdb_connection *out_connection);
(import lib "duckdb_connect" "cdecl")

(define (connect)
	(if (not con)
		(begin
			(set 'state (duckdb_connect db dbc))
			(when (!= state DuckDBSuccess)
				(set 'con nil)
				(throw-error '(nil "error connecting to database!")))
			(set 'con (get-long dbc)))
		(throw-error '(nil "already connected to database"))))

; Closes the specified connection handle
; void duckdb_disconnect(duckdb_connection *connection);
(import lib "duckdb_disconnect" "cdecl")

(define (disconnect)
	(set 'con nil)
	(duckdb_disconnect dbc))

; Destroys the specified result
; void duckdb_destroy_result(duckdb_result *result);
(import lib "duckdb_destroy_result" "cdecl")

(define (destroy-result)		
	(duckdb_destroy_result result))

; Converts the specified value to an int64_t. Returns 0 on failure or NULL.
; int64_t duckdb_value_int64(duckdb_result *result, idx_t col, idx_t row);
(import lib "duckdb_value_int64" "cdecl")

(define (get-int64 ptr col row)
	(duckdb_value_int64 ptr col row))

; Converts the specified value to a string. Returns nullptr on failure or NULL. The result must be freed with free.
; char *duckdb_value_varchar(duckdb_result *result, idx_t col, idx_t row);
(import lib "duckdb_value_varchar" "cdecl")

(define (get-varchar ptr col row)
  (set 'check (duckdb_value_varchar ptr col row))
		(if (zero? check)
			nil 
     (get-string check)))

; Executes the specified SQL query in the specified connection handle. [OUT: result descriptor]
; duckdb_state duckdb_query(duckdb_connection connection, const char *query, duckdb_result *out_result);
(import lib "duckdb_query" "cdecl")

;; @syntax (duck:sql <str-sql>)
;; @param <str-sql> The SQL statement.
;; @return <list>
;; Executes the SQL statement in <str-sql>. For 'select' statements a table
;; of the result set is returned or '()' for the empty set. 
;; On failure a list with (nil <error-message>) is returned
;; 
;; Parameter substitution is not supported (yet) by this module
;;
;; Warning! BLOB's are not supported (yet), will crash if BLOB column is selected
;;
;; @example
;; ; traditional usage 
;; (duck:sql "select * from persons where age > 18;") 

(define (sql qry)
 (local (cols rows ptr error data mask type name)
	(set 'query (rem-slash qry))
	(set 'state (duckdb_query con query result))
  (bind (map list '(cols rows ptr error) (unpack "Ld Ld Ld Ld" (address result))))
  (if (!= state DuckDBSuccess)
		(if (zero? error)
			(throw-error '(nil "unknown error!"))
			(throw-error  (list nil (get-string error)))))
	(set 'col-types '() 'col-names '())
	(dotimes (c cols)
		(bind (map list '(data mask type name) (unpack "Ld Ld Ld Ld" ptr)))
		(push (get-string name) col-names -1)
		(push type col-types -1)
		(inc ptr 32)) ;; duckdb_result structure is packed into 4 longs, so skip 4 longs
	(set 'res '())
	(when (> rows 0)
		(set 'res (map (fn(r) 
                (map (fn(c) (get-values c r (col-types c))) 
                     (sequence 0 (- cols 1))))
			          (sequence 0 (- rows 1)))))
	(destroy-result)
	res))

(define (get-values col row type)
	(cond
		((and (>= type 1) (<= type 5))
			(get-int64 (address result) col row))
 		((= type 12)
			(bigint (get-varchar (address result) col row)))
 		((and (>= type 6) (<= type 7))
			(float (get-varchar (address result) col row)))
 		((and (>= type 8) (<= type 13))
			(get-varchar (address result) col row))
		(true nil)))

; default functor 
(define duck:duck sql) 

;; @syntax (duck:columns <str-table-name>)
;; @return A list of all columns with types of the table
;; Describes table with all columns
;; Returns list with name of column, type,is nullable (yes/no), is key(1/0), default value, extra 

(define (columns table)
	(map (fn(x) (x 0 2)) (duck:sql (string "describe " table " ;"))))

;; @syntax (duck:tables)
;; @return A list of table names.
;; Returns a list of all tables in connected database

(define (tables)
  (duck:sql "select name from sqlite_master() where type = 'table';"))

;; @syntax (duck:colnames)
;; @return A list of column header names.
;; Returns a list of column header names for the last query. This is
;; a function wrapper around the internal variable <tt>duck:col-names</tt>.

(define (colnames) 
duck:col-names)

;; @syntax (duck:coltypes)
;; @return A list of column type names.
;; Returns a list of column type names for the last query. 

(define (coltypes)
	(map (fn(x) (12 (string (DUCKDB_TYPES x)))) col-types))

;; @syntax (duck:create-from-csv <str csv-file> <str table-name> [<str dateformat>])
;; fast csv import, will create new table and guess csv delimiter and types of columns
;; optionally define dateformat , defaults to "%d-%m-%Y"

(define (create-from-csv file table (dateformat "%d-%m-%Y"))
	(duck:sql (string "create table " table " as select * from read_csv_auto('" file "',dateformat='" dateformat"');"))
	(dolist (x (duck:columns table))
		(duck:sql (string "alter table " table " rename  \"" x "\" to " (replace " " x "_") ";")))
	(duck (string  "describe " table ";")) 
)
  

;; @syntax (duck:import-from-csv <file> <str-table-name> [<str-delimiter> <header>])
;; fast csv import into existing table 
;; optionally set delimiter, defaults to comma. 
;; optionally indicate if the file has a header line, defaults to true

(define (import-from-csv file table (del ",") (header true))
	(if header
		(duck:sql (string "copy " table " from '" file "' (DELIMITER '" del "', HEADER );"))  
    (duck:sql (string "copy " table " from '" file "' (DELIMITER '" del "');"))))

;; @syntax (duck:export-to-csv <str-file> <str-table> [<str-delimiter> <header>])
;; export table to csv file 
;; optionally set delimiter, defaults to comma). 
;; optionally indicate if a header line with column names is included, defaults to true

(define (export-to-csv file table (del ",") (header true))
	(if header
		(duck:sql (string "copy " table " to '" file "' (DELIMITER '" del "', HEADER );"))  
        (duck:sql (string "copy " table " to '" file "' (DELIMITER '" del "');"))))

; utility functons for escaping quotes and removing backslashes in strings

; escape quote
(define (esc-quote str)
  (if (string? str) 
		(replace "'" str "''")
		str))

; remove slash
(define (rem-slash str)
	(if (string? str)
	  (replace "\\\"" str (char 34) )
		str))

(context 'MAIN)


; -------------------------------------------------------------------------
;


(define (duck:test)
	(duck:open "DUCKDB-TEST.db")
	(duck:sql "create table if not exists items (name varchar(15), colour varchar, onhand float, created date, counted datetime, cases int);")
; duck:sql is defined as default functor so we can also use only duck
	(duck "insert into items values ('trappist', 'dark', 200.23, '2010-10-10', '2020-08-10 12:00:00', 200);") 
	(duck "insert into items values ('blond', 'light' ,40, '2010-10-10', '2020-08-10 12:00:00', 20);") 
	(duck "insert into items values ('brown', 'dark',202340.1234, '2010-10-10', '2020-08-10 12:00:00', 10);")
	(duck "insert into items values ('ipa', 'light', 0.23, '2000-02-29', '2020-08-10 11:59:59', 100);")  
	(println(duck "select name,created from items order by name limit 2 ;"))
	(duck:export-to-csv "test-csv.csv" "items") 
	(duck "drop table if exists items2;")
	(duck:create-from-csv "test-csv.csv" "items2")
	(println(duck "select distinct colour from items2;"))
	(println(duck:colnames))
	(println(duck:coltypes))
	(duck "delete from items2;")
	(duck:import-from-csv "test-csv.csv" "items2")
	(println(duck:columns "items"))
	(println(duck:tables))
	(println(duck "select count(name) from items;"))
	(duck:close))


; eof ;
