(define (trig-mk proc-tag args) (cons proc-tag args))
(define (trig-proc trg) (eval (car trg)))
(define (trig-args trg) (cdr trg))
(define (trig-invoke trg . more-args)
  (apply (trig-proc trg) (append more-args (trig-args trg))))

;;----------------------------------------------------------------------------
;; Step trigger -- executes a named procedure whan a character steps on it.
;; The procedure should expect a kernel being as the first arg followed by
;; the optional args.
;;----------------------------------------------------------------------------
(define (step-trig-exec ktrig kbeing)
  (let ((trg (gob ktrig)))
    (if (trig-invoke trg kbeing)
        (kern-obj-remove ktrig))))

(define step-trig-ifc
  (ifc '()
       (method 'step step-trig-exec)))

(mk-obj-type 't_step_trig nil nil layer-mechanism step-trig-ifc)

(define (mk-step-trig proc-tag . args)
  (bind (make-invisible (kern-mk-obj t_step_trig 1))
        (trig-mk proc-tag args)))

;;----------------------------------------------------------------------------
;; 'on trigger -- object which executes a named procedure when it gets the 'on
;; signal from something.
;;----------------------------------------------------------------------------
(define (on-trig-exec ktrig)
  (println "on-trig-exec:" ktrig)
  (let ((trg (gob ktrig)))
    (if (trig-invoke trg ktrig)
        (kern-obj-remove ktrig))))

(define on-trig-ifc
  (ifc '()
       (method 'on on-trig-exec)))

(mk-obj-type 't_on_trig nil nil layer-mechanism on-trig-ifc)

(define (mk-on-trig proc-tag args)
  (bind (make-invisible (kern-mk-obj t_on_trig 1))
        (trig-mk proc-tag args)))

