;; A generic timer mech

(define (tmr-mk targ timeout sig)
  (list targ timeout 0 #f sig))

(define (tmr-targ tmr) (car tmr))
(define (tmr-timeout tmr) (cadr tmr))
(define (tmr-count tmr) (caddr tmr))
(define (tmr-on? tmr) (cadddr tmr))
(define (tmr-sig tmr) (list-ref tmr 4))

(define (tmr-set-count! tmr val) (set-car! (cddr tmr) val))
(define (tmr-set-start! tmr val) (set-car! (cdddr tmr) val))

(define (tmr-expired? tmr) (>= (tmr-count tmr) (tmr-timeout tmr)))
(define (tmr-stop! tmr) 
  (tmr-set-count! tmr 0)
  (tmr-set-start! tmr #f))
(define (tmr-inc! tmr) (tmr-set-count! tmr (+ 1 (tmr-count tmr))))

(define (ktmr-start! ktmr) 
  (let ((tmr (kobj-gob-data ktmr)))
    (tmr-set-count! tmr 0)
    (tmr-set-start! tmr #t)))

(define (ktmr-exec ktmr)
  (let ((tmr (kobj-gob-data ktmr)))
    (display "tmr-exec")(newline)
    (if (tmr-on? tmr)
        (begin
          (display "tmr-on")(newline)
          (tmr-inc! tmr)
          (if (tmr-expired? tmr)
              (let* ((tag (tmr-targ tmr))
                     (targ (safe-eval tag)))
                (display "timer-expired")(newline)
                (display "timer-sig:")(display (tmr-sig tmr))(newline)
                (tmr-stop! tmr)
                (if (notnull? tag)
                    (signal-kobj targ (tmr-sig tmr) targ ktmr))))))))

(define timer-ifc
  (ifc nil
       (method 'exec ktmr-exec)
       (method 'start ktmr-start!)
       ))

(mk-obj-type 't_timer "timer" '() layer-mechanism timer-ifc)

(define (mk-timer target-tag timeout sig)
  (bind (kern-mk-obj t_timer 1)
        (tmr-mk target-tag timeout sig)))
