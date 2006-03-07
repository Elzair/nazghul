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
		
(define (mk-lever-on dest-tag)
  (bind (kern-mk-obj t_lever 1)
        (bim-mk #t dest-tag nil)))

(define (mk-lever-with-id dest-tag id)
  (bind (kern-mk-obj t_lever 1)
         (bim-mk #f dest-tag id)))


;;----------------------------------------------------------------------------
;; Disguised lever
;;----------------------------------------------------------------------------
(define (disg-lvr-state on? klvr)
  (let ((bim (kobj-gob-data klvr)))
    (state-mk (bim-members bim) #f pclass-none 0)))

(define disg-lvr-ifc
  (ifc bim-ifc
       (method 'handle bim-toggle)
       (method 'state disg-lvr-state)))

(mk-obj-type 't_disg_lvr nil '() layer-mechanism disg-lvr-ifc)

(define (mk-disg-lvr dest-tag sprite-tag)
  (bind (kern-mk-obj t_disg_lvr 1)
        (bim-mk #f dest-tag sprite-tag)))
