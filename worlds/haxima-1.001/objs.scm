;; ----------------------------------------------------------------------------
;; objs.scm -- basic object types and hooks
;;----------------------------------------------------------------------------

;; Create the default g)et handler
(define (obj-get actor subject)
  (kern-obj-remove subject)
  (kern-obj-put-into subject actor))

;; Make the basic object interface which supports g)et
(define obj-ifc
  (ifc '()
       (method 'get (lambda (kobj getter)
                      (kern-obj-remove kobj)
                      (kern-obj-put-into kobj getter)))))

;; Make a simple container (no ifc)
(mk-obj-type 't_small_wooden_chest "small wooden chest" s_small_wooden_chest 
             layer-container nil)
