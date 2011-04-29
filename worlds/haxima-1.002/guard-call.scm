;;----------------------------------------------------------------------------
;; A guard-call is an invisible object that attracts guards.
;;
;; The gob is just the duration in turns. Every turn it decrements, and when it
;; reaches zero the call is automatically removed.

;; gob
(define (guard-call-mk dur) (list dur))
(define (guard-call-expired? gob) (not (> (car gob) 0)))
(define (guard-call-decr! gob) (set-car! gob (- (car gob) 1)))

;; ifc
(define (guard-call-exec kobj)
  (let ((gob (gob kobj)))
    (if (guard-call-expired? gob)
        (kern-obj-remove kobj)
        (guard-call-decr! gob))))

(define guard-call-ifc
  (ifc '()
       (method 'exec guard-call-exec)))

;; type
(mk-obj-type 't_guard_call nil nil layer-none guard-call-ifc)

;; ctor
(define (mk-guard-call dur)
  (println "mk-guard-call")
  (bind (kern-mk-obj t_guard_call 1)
        (guard-call-mk dur)))
  