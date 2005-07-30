;; A lever is a basic binary mechanism.

(define (lever-state on?)
  (if on?
      (state-mk 's_R_lever_up #f pclass-none 0)
      (state-mk 's_R_lever_down #f pclass-none 0)))

(define lever-ifc
  (ifc bim-ifc
       (method 'handle bim-toggle)
       (method 'state lever-state)))

(mk-obj-type 't_lever "lever" '() layer-mechanism lever-ifc)

(define (mk-lever dest-tag)
  (bind (kern-mk-obj t_lever 1)
        (bim-mk #f dest-tag nil)))
