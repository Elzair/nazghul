;; Accessors - a drawbridge gob is simply its sprite in the 'on state
(define (drawbridge-sprite-tag drawbridge) 
  (display "drawbridge-sprite-tag drawbridge:")(display drawbridge)(newline)
  drawbridge)

;; Extend the bim interface to support the 'signal message
(define (drawbridge-state on? kobj)
  (let* ((bim (gob-data (kobj-gob kobj))))
    (if on?
        (state-mk (drawbridge-sprite-tag (bim-members bim)) #f pclass-bridge 0)
        (state-mk nil #f pclass-none   0))))

(define drawbridge-ifc
  (ifc bim-ifc
       (method 'signal bim-toggle)
       (method 'state drawbridge-state)
       ))

;; Make a kernel drawbridge type
(mk-obj-type 'TF_drawbridge "drawbridge" nil layer-tfeat drawbridge-ifc)

;; Define a constructor
(define (mk-drawbridge dir)
  (case dir
    ((north south) (bind (kern-mk-obj TF_drawbridge 1) 
                         (bim-mk #f '() 's_ns_bridge)))
    (else          (bind (kern-mk-obj TF_drawbridge 1) 
                         (bim-mk #f '() 's_ew_bridge)))))
