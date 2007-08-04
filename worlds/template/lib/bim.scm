;; bim - binary mechanism

(define (state-mk sprite-tag opacity pclass light)
  (list sprite-tag opacity pclass light))
(define (state-sprite state) (eval (car state)))
(define (state-opacity state) (cadr state))
(define (state-pclass state) (caddr state))
(define (state-light state) (cadddr state))

;; ctor
(define (bim-mk on? port members)
  ;;(display "bim-mk")(newline)
  (list on? port #f members))

;; accessors
(define (bim-on? bim) (car bim))
(define (bim-port bim) (cadr bim))
(define (bim-active? bim) (caddr bim))
(define (bim-members bim) (cadddr bim))

;; mutators
(define (bim-set-on! bim val) (set-car! bim val))
(define (bim-set-active! bim val) (set-car! (cddr bim) val))

;; helpers
(define (bim-send-signal kobj sig)
  (let ((bim (gob-data (kobj-gob kobj))))
    (if (not (bim-active? bim))
        (let ((port (bim-port bim)))
          (if (and (not (null? port)) 
                   (defined? port))
              (begin
                (bim-set-active! bim #t)
                ((kobj-ifc (eval port)) sig (eval port) kobj)
                (bim-set-active! bim #f)))))))

(define (bim-change-state kobj khandler on?)
  ;;(display "bim-change-state")(newline)
  (let ((bim (gob-data (kobj-gob kobj))))
    (bim-set-on! bim on?)
    (let ((state ((kobj-ifc kobj) 'state on? kobj)))
      ;;(display "state:")(display state)(newline)
      (kern-obj-set-sprite kobj (state-sprite state))
      (kern-obj-set-opacity kobj (state-opacity state))
      (kern-obj-set-pclass kobj (state-pclass state))
      (kern-obj-set-light kobj (state-light state))
      )))

;; handlers
(define (bim-on kobj khandler) 
  ;(display "bim-on")(newline)
  (bim-change-state kobj khandler #t 'on)
  (bim-send-signal kobj 'on)
  )

(define (bim-off kobj khandler) 
  ;(display "bim-off")(newline)
  (bim-change-state kobj khandler #f 'on)
  (bim-send-signal kobj 'off)
  )

(define (bim-toggle kobj khandler)
  ;;(display "bim-toggle")(newline)
  (let ((bim (gob-data (kobj-gob kobj))))
    (if (bim-on? bim) 
        (bim-off kobj khandler)
        (bim-on kobj khandler))))

(define (bim-init kobj)
  (let ((bim (gob-data (kobj-gob kobj))))
    (if (bim-on? bim)
        (bim-on kobj '())
        (bim-off kobj '()))))

(define (bim-is-on? kobj)
  (let ((bim (kobj-gob-data kobj)))
    (bim-on? bim)))

;; ifc - extensions must add a 'state message handler
(define bim-ifc
  (ifc '()
       (method 'on bim-on)
       (method 'off bim-off)
       (method 'toggle bim-toggle)
       (method 'init bim-init)
       (method 'is-on? bim-is-on?)
       ))

