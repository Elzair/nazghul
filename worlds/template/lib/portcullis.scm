;; A portcullis is a trivial extension of the binary mechanism. It responds to
;; a generic 'signal by toggling its state.

;; Extend the bim interface to support the 'signal message
(define (portcullis-state on?)
  (if on?
      (state-mk 's_portcullis_up #f pclass-none 0)
      (state-mk 's_portcullis_down #f pclass-bars 0)))

(define (kportcullis-manual kobj khandler)
  (kern-log-msg "Portcullis won't budge!"))

(define portcullis-ifc
  (ifc bim-ifc
       (method 'open kportcullis-manual)
       (method 'close kportcullis-manual)
       (method 'open-remote bim-on)
       (method 'close-remote bim-off)	   
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
