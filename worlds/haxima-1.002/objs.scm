;; ----------------------------------------------------------------------------
;; objs.scm -- basic object types and hooks
;;----------------------------------------------------------------------------

;; Make the basic object interface which supports g)et
(define obj-ifc
  (ifc '()
       (method 'get kobj-get)))


;; misc object types that just don't fit anywhere else
(mk-obj-type 't_corpse "corpse" s_corpse layer-item nil)
(define (mk-corpse) (kern-mk-obj t_corpse 1))
