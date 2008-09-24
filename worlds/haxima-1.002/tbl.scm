;; (*tbl* (k1 v1) (k2 v2) ...)

(define (tbl-mk)
  (list '*tbl*))

;; the val for key (#f if none)
(define (tbl-get tbl key)
  (let ((kvpair (assoc key (cdr tbl))))
    (if kvpair
        (cdr kvpair)
        nil)))

;; add key/val or replace the current val of key
(define (tbl-set! tbl key val)
  (let ((kvpair (assoc key (cdr tbl))))
    (if kvpair
        (set-cdr! kvpair val)
        (set-cdr! tbl 
                  (cons (cons key val) 
                        (cdr tbl))))))

;; append val to the value of key; if key is not there make a new list with
;; just val
(define (tbl-append! tbl key val)
  (let ((entry (assoc key (cdr tbl))))
    (if (or (not entry)
            (not (pair? (cdr entry))))
        (tbl-set! tbl key (list val))
        (set-cdr! entry (cons val (cdr entry))))))

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
		(tbl-set! tbl (car entrydata) (car (cdr entrydata)))
		(tbl-set-all! tbl (cddr entrydata))
	))

(define (tbl-build . entrydata)
	(let ((tbl (tbl-mk)))
		(tbl-set-all! tbl entrydata)
		tbl
	))
          