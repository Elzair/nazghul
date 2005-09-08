;; A portcullis is a trivial extension of the binary mechanism. It responds to
;; a generic 'signal by toggling its state.

;; Extend the bim interface to support the 'signal message
(define (portcullis-state on?)
  (if on?
      (state-mk 's_portcullis_up #f pclass-none 0)
      (state-mk 's_portcullis_down #f pclass-wall 0)))

(define portcullis-ifc
  (ifc bim-ifc
       (method 'open bim-on)
       (method 'close bim-off)
       (method 'signal bim-toggle)
       (method 'state portcullis-state)
       ))

;; Make a kernel portcullis type
(mk-obj-type 't_portcullis "portcullis" nil layer-mechanism portcullis-ifc)

;; Define a constructor
(define (mk-connected-portcullis dest-tag)
  (bind (kern-mk-obj t_portcullis 1) 
        (bim-mk #f dest-tag nil)))
  
(define (mk-portcullis) 
  (mk-connected-portcullis nil))

(define (mk-open-portcullis)
  (bind (kern-mk-obj t_portcullis 1) 
        (bim-mk #t nil nil)))

;; Make a generic terrain blitter
; (define (tb-mk tag x y w h ter)
;   (list tag x y w h ter))
; (define (tb-terrain tb) (list-ref tb 5))
; (define (tb-place tb) (eval (car tb)))
; (define (tb-x tb) (cadr tb))
; (define (tb-y tb) (caddr tb))
; (define (tb-w tb) (cadddr tb))
; (define (tb-h tb) (list-ref tb 4))

; (define (terrain-blitter-exec kmech ksrc)
;   (let ((tb (kobj-gob-data kmech)))
;     (fill-terrain (tb-terrain tb)
;                   (tb-place tb)
;                   (tb-x tb)
;                   (tb-y tb)
;                   (tb-w tb)
;                   (tb-h tb))))

; (define terrain-blitter-ifc
;   (ifc nil
;        (method 'on terrain-blitter-exec)))

; (mk-obj-type 't_terrain_blitter nil nil layer-none terrain-blitter-ifc)

; (define (mk-terrain-blitter dest-place-tag x y w h kterrain)
;   (bind (kern-mk-obj t_terrain_blitter 1)
;         (list dest-place-tag x y w h kterrain)))
