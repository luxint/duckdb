;; @module duckpivot.lsp
;; @description database interface modules
;; @version 0.1 initial version 19 july 2020

;; <h2>Module for duckdb pivot command</h2>
;; To use this module include the following 'load' statement at the
;; beginning of the program file:
;; <pre>
;; (load "/usr/local/share/duckdb/duckpivot.lsp")
;; </pre>
;; Test the module:
;; <pre>
;; (test-duckpivot)
;; </pre>


(load (string (env "HOME") "/duckdb/duck.lsp"))

(context 'pivot)
(define max-column 35)
(define min-column 10)
(define terminalwidth (if (env "TERM") (int ((exec "tput cols")0)) 80))

;; @syntax (pivot <str-table> <str-rows> <str-columns> <str-values> [<str-aggregate functions> <bool-pretty-print>])
;; @return a 'pivoted' list or pretty-printed pivot-table with header
;;  

(define (pivot:pivot table rows cols vals (aggregates "") (pp true))
	(set 'p-rows (parse rows ", ") 'p-cols (parse cols ", ") 'p-vals (parse vals ", "))
	(set 'o-cols (join (map (fn(x) ((parse x " as ") 0)) p-cols) ", "))
	(set 'agg (parse (dup "sum " (length p-vals))))
	(dolist (x (parse aggregates ", "))
		(setq (agg $idx) x)) 
	(set 'sql1 (string "select distinct " cols " from " table " as temp1 order by " o-cols " ;"))
    (set 'columns  (duck sql1))
    (set 'colnames1 (duck:colnames))	
    (set 'sql (string "select " rows "," (make-case) " from " table " as temp1 group by " rows " order by " rows ";"))
	(set 'result (duck sql))
	(set 'colnames (duck:colnames)) 
    (set 'widths (gen-widths2 table))
	(if pp
	  (display (append (make-header (length p-rows) colnames) result))
	  result)
)


(define (make-case)
 (set 'stmt "")
 (set 'p-cols (map (fn(x) ((parse x " as ") 0)) p-cols)) 
 (set 'p-vals (map (fn(x) (let (px (parse x " as ")) (list (first px) (first (or (rest px) (list(first px))))))) p-vals))  
   (dolist (head columns)
		(set 'head (map string head)) 
	   (dolist (v p-vals)	
	     (push (string (agg $idx) "(case when "
					             (join (map (fn(x y) (string x " = '" (qe y) "'")) p-cols head)
						                 " and ") 
					            " then " (v 0) " end) as '"  (qe (join (append head (list (v 1))) ", ")) "',")
				    stmt -1)))
 (chop stmt)
)

; escape quote
(define (qe str)
  (if (string? str) 
		(replace "'" str "''")
		str))

(define (make-header rw header)
	(set 'lst '())
	(set 'ch (+ (length colnames1) 1))
	(set 'ph (map (fn(c)  (parse c ", ")) header)) 	
	(dotimes (x ch)
		(set 't-list '())
     	(dotimes (y (length header))		
			(cond ((and (= x (- ch 1)) (< y rw))
						 (push (upper-case (ph y 0)) t-list -1))
						((< y (- rw 1))
						 (push "" t-list -1))
						((and (= y (- rw 1)) (< x (- ch 1)))
						 (push (upper-case  (colnames1 x)) t-list -1))
						(true
						 (push (ph y x) t-list -1))))
		 (push t-list lst -1))
		(push (map (fn(x) (dup "-" x)) widths)	lst -1))				

(define (display lst (d 0))
	(dolist (l lst)
		(println (0 terminalwidth(join (map (fn(x) (fmt x (max min-column (min max-column (widths $idx))) d)) l)))))nil)

(define (fmt val w d)
	(set 'val (or val " ")) ; for nil values
	(if (number? val)
		(set 'val (format (string "%" (- w d) "." d "f") val)))
	  (if (> (length val) w)
		 (string (0 (- w 1) val) "~|")
		 (string (format (string "%-" w "s") val)"|")))


(define (row-width tbl)
	(map (fn(x) (+ 1 ((duck (string "select max(length(cast (" x " as text))) from " tbl " as temp1;"))0 0))) p-rows))

(define (col-width)
	(+ 1 (apply max (map length (append p-vals (map first columns)))))) 

(define (gen-widths table)
	(append (row-width table) (dup (col-width) (* (length p-vals) (length columns)))))

(define (gen-widths2 table)
	(append (row-width table) (map (fn(x) (apply max (map length (parse x ", ")))) ((length p-rows) colnames))))
 
(context 'MAIN)


(define (pivot:test)
(duck:open)
(duck:create-from-csv (string (env "HOME") "/duckdb/test/1500000 Sales Records.csv") "sales" "%m/%d/%Y")  
(duck "describe sales;")
;(pivot "sales" "sales_channel, item_type" "region" "total_revenue" )
(pivot "sales" "region" "extract('year' from ship_date) as years" "total_revenue - total_cost as profit" )
;(duck:close)
)



	
