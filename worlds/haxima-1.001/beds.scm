;; ----------------------------------------------------------------------------
;; Beds
;;
;; Player can k)amp in beds to sleep in town. The kernel identifies bed objects
;; by their layer.
;;
;; ----------------------------------------------------------------------------

(mk-obj-type 't_bed    ;; tag
             "bed"     ;; name
             s_bed     ;; sprite
             layer-bed ;; layer
             nil       ;; interface
             )

(define (mk-bed) (kern-mk-obj t_bed 1))
