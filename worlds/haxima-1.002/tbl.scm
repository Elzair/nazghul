(define (tbl-mk)
  (list '*tbl*))

;; the val for key (#f if none)
(define (tbl-get tbl key)
  (let ((kvpair (assoc key (cdr tbl))))
    (if kvpair
        (cdr kvpair)
        #f)))

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
