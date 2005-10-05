;;----------------------------------------------------------------------------
;; Step trigger -- executes a named procedure whan a character steps on it.
;; The procedure should expect a kernel being as the first arg followed by
;; the optional args.
;;----------------------------------------------------------------------------
(define (step-trig-mk proc-tag args) (list proc-tag args))
(define (step-trig-invoke stp kbeing)
  (apply (eval (car stp)) (cons kbeing (cadr stp))))

(define (step-trig-exec ktrig kbeing)
  ;;(display "step-trig-exec")(newline)
  (let* ((trg (kobj-gob-data ktrig)))
    (if (step-trig-invoke trg kbeing)
        (kern-obj-remove ktrig))))

(define step-trig-ifc
  (ifc '()
       (method 'step step-trig-exec)))

(mk-obj-type 't_step_trig nil nil layer-mechanism step-trig-ifc)

(define (mk-step-trig proc-tag args)
  (bind (make-invisible (kern-mk-obj t_step_trig 1))
        (step-trig-mk proc-tag args)))
