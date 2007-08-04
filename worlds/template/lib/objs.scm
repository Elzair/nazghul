;; ----------------------------------------------------------------------------
;; objs.scm -- basic object types and hooks
;;----------------------------------------------------------------------------

;; Make the basic object interface which supports g)et
(define obj-ifc
  (ifc '()
       (method 'get kobj-get)))
