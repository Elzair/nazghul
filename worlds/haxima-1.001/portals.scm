(define (portal-mk place x y) (list place x y))
(define (portal-place portal) (safe-eval (car portal)))
(define (portal-coords portal) (cons (portal-place portal) (cdr portal)))

(define (portal-step kportal kstepper)
  (let ((portal (kobj-gob-data kportal)))
    (kern-obj-relocate kstepper
                       (portal-coords portal)
                       nil)))

(define portal-ifc
  (ifc '()
       (method 'enter portal-step)))

(define auto-portal-ifc
  (ifc '()
       (method 'step portal-step)))

(mk-obj-type 't_ladder_down "ladder leading down" s_ladder_down layer-mechanism portal-ifc)
(mk-obj-type 't_ladder_up "ladder leading up" s_ladder_up layer-mechanism portal-ifc)
(mk-obj-type 't_trap_door "trap door" '() layer-mechanism auto-portal-ifc)

;; mk-portal -- generic helper constructor
(define (mk-portal type place-tag x y)
  (bind (kern-mk-obj type 1)
        (portal-mk place-tag x y)))
 
;; specific portal constructors
(define (mk-ladder-down place-tag x y) (mk-portal t_ladder_down place-tag x y))
(define (mk-ladder-up place-tag x y) (mk-portal t_ladder_up place-tag x y))
(define (mk-trap-door place-tag x y) (mk-portal t_trap_door place-tag x y))
