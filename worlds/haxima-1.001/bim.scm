;; bim - binary mechanism

(define (state-mk sprite-tag opacity pmask)
  (list sprite-tag opacity pmask))
(define (state-sprite state) (eval (car state)))
(define (state-opacity state) (cadr state))
(define (state-pmask state) (caddr state))

;; ctor
(define (bim-mk on? port)
  ;;(display "bim-mk")(newline)
  (list on? port #f))

;; accessors
(define (bim-on? bim) (car bim))
(define (bim-port bim) (cadr bim))
(define (bim-active? bim) (caddr bim))

;; mutators
(define (bim-set-on! bim val) (set-car! bim val))
(define (bim-set-active! bim val) (set-car! (cddr bim) val))

;; helpers
(define (bim-send-signal kobj sig)
  ;;(display "bim-send-signal ")(display sig)(newline)
  (let ((bim (gob-data (kobj-gob kobj))))
    ;;(display bim)(newline)
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
    (let ((state ((kobj-ifc kobj) 'state on?)))
      (kern-obj-set-sprite kobj (state-sprite state))
      (kern-obj-set-opacity kobj (state-opacity state))
      (kern-obj-set-pmask kobj (state-pmask state)))))

;; handlers
(define (bim-on kobj khandler) 
  ;;(display "bim-on")(newline)
  (bim-change-state kobj khandler #t 'on)
  (bim-send-signal kobj 'on)
  )

(define (bim-off kobj khandler) 
  ;;(display "bim-off")(newline)
  (bim-change-state kobj khandler #f 'on)
  (bim-send-signal kobj 'off)
  )

(define (bim-toggle kobj khandler)
  (let ((bim (gob-data (kobj-gob kobj))))
    (if (bim-on? bim) 
        (bim-off kobj khandler)
        (bim-on kobj khandler))))

(define (bim-init kobj)
  (let ((bim (gob-data (kobj-gob kobj))))
    (if (bim-on? bim)
        (bim-on kobj '())
        (bim-off kobj '()))))

;; ifc - extensions must add a 'state message handler
(define bim-ifc
  (ifc '()
       (method 'on bim-on)
       (method 'off bim-off)
       (method 'toggle bim-toggle)
       (method 'init bim-init)
       ))

