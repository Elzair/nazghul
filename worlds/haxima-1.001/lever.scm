;; A lever is a basic binary mechanism.

(define (lever-state on?)
  (if on?
      (list 's_R_lever_up #f pmask-all)
      (list 's_R_lever_down #f pmask-all)))

(define lever-ifc
  (ifc bim-ifc
       (method 'handle bim-toggle)
       (method 'state lever-state)))

(mk-obj-type 't_lever "lever" '() layer-mechanism lever-ifc)

(define (mk-lever dest-tag)
  (bind (kern-mk-obj t_lever 1)
        (bim-mk #f dest-tag)))
