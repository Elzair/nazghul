;; stone lantern - simple binary mechanism which emits light when turned on

;; Called by bim-change-state to get the current state variables:
(define (lantern-state on?)
  (display "lantern-state:")(display on?)(newline)
  (if on?
      (state-mk 's_stone_lantern_on #f pclass-mountains 512)
      (state-mk 's_stone_lantern_off #f pclass-mountains 0)))

;; Signal handler interface:
(define lantern-ifc
  (ifc bim-ifc
       (method 'handle bim-toggle)
       (method 'state lantern-state)))


;; Object type (class):
(mk-obj-type 'tf_stone_lantern ;; tag
             "stone lantern"   ;; name
             nil               ;; sprite
             layer-mechanism   ;; layer
             lantern-ifc       ;; interface
             )

;; Constructor
(define (mk-stone-lantern)
  (bind (kern-mk-obj tf_stone_lantern 1)
        (bim-mk #f nil nil)))
