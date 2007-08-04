
;; The 'init signal is sent after the instance is created. This is our chance
;; to initialize fields to non-default values. Note: all the pclass values are
;; defined in game.scm.
(define (bridge-init kbridge)
  (kern-obj-set-pclass kbridge pclass-bridge))

;; Define the interface for the type
(define bridge-ifc
  (ifc '()
       (method 'init bridge-init)))

;; Make an object type. This is like a "class" in OO languages.
(mk-obj-type 'TF_ew_bridge  ;; tag
             "bridge"       ;; name
             s_ew_bridge    ;; sprite
             layer-tfeat    ;; stacking layer
             bridge-ifc     ;; interface
             )

(mk-obj-type 'TF_ns_bridge  ;; tag
             "bridge"       ;; name
             s_ns_bridge    ;; sprite
             layer-tfeat    ;; stacking layer
             bridge-ifc     ;; interface
             )
;; ----------------------------------------------------------------------------
;; mk-bridge -- 'dir' is the orientation, one of the four cardinal directions
;; and should be prepended by a single tick when called, as in 'north 'south
;; 'east and 'west
;; ----------------------------------------------------------------------------
(define (mk-bridge dir)
  (case dir
    ((north) (bind (kern-mk-obj TF_ns_bridge 1) nil))
    ((south) (bind (kern-mk-obj TF_ns_bridge 1) nil))
    (else (bind (kern-mk-obj TF_ew_bridge 1) nil))))
