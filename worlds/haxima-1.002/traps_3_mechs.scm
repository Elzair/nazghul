;; Define a mech that will control the 12 portcullisses in traps_3.

;; Give it a list of 7 lists of 12 bools for sets:
;;   ((#t #t #t #t #t #t #f #f #f #f #f #f) ... ))
(define (t3-ctrl-mk port-tags sets) (cons port-tags sets))
(define (t3-ctrl-ports ctrl) (car ctrl))
(define (t3-ctrl-sets ctrl) (cdr ctrl))

;; Note: can't use tagged levers and expect the tags to work in the case
;; statement here. This is due to what I might describe as an "aliasing"
;; problem, where the tags and the klvr both refer to the same thing but are
;; not themselves the same thing. So use a special extension to the lever which
;; keeps an id in the bim members "field".
(define (t3-ctrl-exec kself klvr)  
  ;;(display "t3-ctrl-exec")(newline)
  (let* ((ctrl (kobj-gob-data kself))
         (ports (t3-ctrl-ports ctrl))
         (sets (t3-ctrl-sets ctrl))
         (n (bim-members (kobj-gob-data klvr))))
  (define (change port on?)
    ;; use safe-eval on the port because on startup all the levers fire, and
    ;; the target portcullisses might not be defined yet depending on the order
    ;; of declaration.
    (let ((kport (safe-eval port)))
      (if (notnull? kport)
          (if on?
              ((kobj-ifc (eval port)) 'open-remote (eval port) kself)
              ((kobj-ifc (eval port)) 'close-remote (eval port) kself)))))
  (map change ports (list-ref sets n))))

(define t3-ctrl-ifc
  (ifc '()
       (method 'on t3-ctrl-exec)
       (method 'off t3-ctrl-exec)
       ))

(mk-obj-type 't_t3_ctrl nil nil layer-none t3-ctrl-ifc)

(define (mk-t3-ctrl ports sets)
  (bind (kern-mk-obj t_t3_ctrl 1)
        (t3-ctrl-mk ports sets)))
