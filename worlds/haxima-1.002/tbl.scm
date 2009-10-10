;; (*tbl* (k1 v1) (k2 v2) ...)

(define (tbl-mk)
  (list '*tbl*))

;; the val for key (#f if none)
(define (tbl-get tbl key)
  (let ((kvpair (assoc key (cdr tbl))))
    (if kvpair
        (cadr kvpair)
        nil)))

;; add key/val or replace the current val of key
(define (tbl-set! tbl key val)
  (let ((kvpair (assoc key (cdr tbl))))
    (if kvpair
        (set-cdr! kvpair (list val))
        (set-cdr! tbl 
                  (cons (cons key (list val)) 
                        (cdr tbl))))))

;; append val to the value of key;
;; if key is not there make a new list with just val
;; if current value is not a list, converts it to a list first
(define (tbl-append! tbl key val)
  (let ((entry (assoc key (cdr tbl))))
     (cond ((or (not entry)
            	(not (pair? (cdr entry))))
        	(tbl-set! tbl key (list val)))
        ((not (pair? (cadr entry)))
        	(set-cdr! entry (list (cons val (cdr entry)))))
        (#t 
        	(set-cdr! entry (list (cons val (cadr entry))))))))

;; run a procedure on each value in the table
(define (tbl-for-each-val fx tbl)
  (for-each (lambda (entry)
              (println "tbl-for-each-val:entry=" entry)
              (println "cdr=" (cdr entry))
              (apply fx (cdr entry)))
            (cdr tbl)))

;; remove the entry that matches key
(define (tbl-rm! tbl key)
  (if (pair? (cdr tbl))
      (if (equal? key (caadr tbl))
          (set-cdr! tbl (cddr tbl))
          (tbl-rm! (cdr tbl) key))))

;; set table values from name/value list
(define (tbl-set-all! tbl entrydata)
	(if (not (null? entrydata))
		(begin
			(tbl-set! tbl (car entrydata) (car (cdr entrydata)))
			(tbl-set-all! tbl (cddr entrydata))
		)
	))

(define (tbl-build . entrydata)
	(let ((tbl (tbl-mk)))
		(tbl-set-all! tbl entrydata)
		tbl
	))

(define (is-tbl? tbl)
  (and (pair? tbl)
       (equal? (car tbl) '*tbl*)
       )
  )
	
	