
;; Make a door class.
(define (door-open? door) (car (gob-data door)))
(define (door-timeout door) (cadr (gob-data door)))
(define (door-port door) (caddr (gob-data door)))
(define (door-active? door) (cadddr (gob-data door)))
(define (door-locked? door) (car (cddddr (gob-data door))))
(define (door-magic-locked? door) (list-ref (gob-data door) 5))

(define (door-set-open door val) (set-car! (gob-data door) val))
(define (door-set-timeout! door time) (set-car! (cdr (gob-data door)) time))
(define (door-set-port! door port) (set-car! (cddr (gob-data door)) port))
(define (door-set-active! door val) (set-car! (cdddr (gob-data door)) val))
(define (door-set-locked! door val) (set-car! (cddddr (gob-data door)) val))
(define (door-set-magic-locked! door val) 
  (list-set-ref! (gob-data door) 5 val))

(define (door-send-signal kdoor sig)
  (let ((door (kobj-gob kdoor)))
    (if (not (door-active? door))
        (begin
          (let ((port (door-port door)))
            (door-set-active! door #t)
            (if (not (null? port))
                (begin
                  ((kobj-ifc (eval port)) sig (eval port) kdoor)))
            (door-set-active! door #f))))))

(define (door-update-kstate kdoor)
  (let ((door (kobj-gob kdoor)))
    ;;(display "door-update-kstate:")(display door)(newline)
    (cond ((door-magic-locked? door) 
           (kern-obj-set-sprite kdoor 
                                s_magically_locked_solid_wood_door_in_stone)
           (kern-obj-set-opacity kdoor #t)
           (kern-obj-set-pclass kdoor pclass-wall))
          ((door-locked? door) 
           (kern-obj-set-sprite kdoor s_locked_solid_wood_door_in_stone)
           (kern-obj-set-opacity kdoor #t)
           (kern-obj-set-pclass kdoor pclass-wall))
          ((door-open? door) 
           (kern-obj-set-sprite kdoor s_open_solid_wood_door_in_stone)
           (kern-obj-set-opacity kdoor #f)
           (kern-obj-set-pclass kdoor pclass-none))
          (else 
           (kern-obj-set-sprite kdoor s_closed_solid_wood_door_in_stone)
           (kern-obj-set-opacity kdoor #t)
           (kern-obj-set-pclass kdoor pclass-wall))))
  (kern-map-set-dirty)
  kdoor)

(define (door-open kdoor khandler) 
  (let ((door (kobj-gob kdoor)))
    (cond 
     ((door-magic-locked? door)
      (kern-log-msg "Magically locked!\n")
      #f)
     ((door-locked? door)
      (kern-log-msg "Locked!\n")
      #f)
      (else
       (door-set-open door #t)
       (door-set-timeout! door 10)
       (door-update-kstate kdoor)
       (door-send-signal kdoor 'open)
       #t))))
  
(define (door-close kdoor khandler)
  ;;(display "door-close")(newline)
  (let ((door (kobj-gob kdoor)))
    (door-set-open door #f)
    (door-set-timeout! door 0)
    (door-update-kstate kdoor)
    (door-send-signal kdoor 'close)))

(define (door-lock kdoor khandler)
  (let ((door (kobj-gob kdoor)))
    ;;(display "door-lock:")(display door)(newline)
    (cond ((door-open? door) (kern-log-msg "Not closed!\n"))
          ((door-locked? door) (kern-log-msg "Already locked!\n"))
          (else
           (door-set-locked! door #t)
           (door-update-kstate kdoor)))))

(define (door-unlock kdoor khandler)
  (let ((door (kobj-gob kdoor)))
    ;;(display "door-unlock:")(display door)(newline)
    (cond ((door-open? door) (kern-log-msg "Not closed!\n"))
          ((not (door-locked? door)) (kern-log-msg "Not locked!\n"))
          (else
           (door-set-locked! door #f)
           (door-update-kstate kdoor)))))

(define (door-magic-lock kdoor khandler)
  (let ((door (kobj-gob kdoor)))
    ;;(display "door-magic-lock:")(display door)(newline)
    (cond ((door-open? door) (kern-log-msg "Not closed!\n"))
          ((door-magic-locked? door) 
           (kern-log-msg "Already magically locked!\n"))
          (else
           (door-set-magic-locked! door #t)
           (door-update-kstate kdoor)))))

(define (door-magic-unlock kdoor khandler)
  (let ((door (kobj-gob kdoor)))
    ;;(display "door-magic-unlock:")(display door)(newline)
    (cond ((door-open? door) (kern-log-msg "Not closed!\n"))
          ((not (door-magic-locked? door)) 
           (kern-log-msg "Not magically locked!\n"))
          (else
           (door-set-magic-locked! door #f)
           (door-update-kstate kdoor)))))

(define (door-handle kdoor khandler)
  (let ((door (kobj-gob kdoor)))
    (if (door-open? door) 
        (door-close kdoor khandler)
        (door-open kdoor khandler))))

(define (door-exec kdoor)
  (let ((door (kobj-gob kdoor)))
    (if (door-open? door)
        (let ((timeout (door-timeout door)))
          (cond ((> timeout 1) (door-set-timeout! door (- timeout 1)))
                ((= timeout 1) (door-close kdoor '())))))))

; (define (door-init kdoor)
;    (let ((door (kobj-gob kdoor)))
;      (cond ((door-open? door) 
;             (kern-obj-set-sprite kdoor s_open_solid_wood_door_in_stone)
;             (kern-obj-set-opacity kdoor #f)
;             (kern-obj-set-pclass kdoor pclass-land))
;            ((door-locked? door)
;             (kern-obj-set-sprite kdoor s_locked_solid_wood_door_in_stone)
;             (kern-obj-set-opacity kdoor #t)
;             (kern-obj-set-pclass kdoor pclass-solid))
;            (else
;             (kern-obj-set-sprite kdoor s_closed_solid_wood_door_in_stone)
;             (kern-obj-set-opacity kdoor #t)
;             (kern-obj-set-pclass kdoor pclass-solid))))
;    kdoor)

(define (door-connect kobj kto-tag)
  (let ((door (kobj-gob kobj)))
    (door-set-port! door kto-tag)))


(define door-ifc
  (ifc '()
       (method 'exec door-exec)
       (method 'handle door-handle)
       (method 'open door-open)
       (method 'close door-close)
       (method 'init door-update-kstate)
       (method 'connect door-connect)
       (method 'lock door-lock)
       (method 'unlock door-unlock)
       (method 'magic-lock door-magic-lock)
       (method 'magic-unlock door-magic-unlock)
       ))

(mk-obj-type 't_door "door" s_closed_solid_wood_door_in_stone layer-mechanism 
             door-ifc)

;; Make a door constructor for virgin doors
(define (mk-door)
  (bind (kern-mk-obj t_door 1) (list #f 0 '() #f #f #f)))

(define (mk-locked-door)
  (bind (kern-mk-obj t_door 1) (list #f 0 '() #f #t #f)))

(define (mk-magically-locked-door)
  (bind (kern-mk-obj t_door 1) (list #f 0 '() #f #f #t)))

(define (mk-connected-door dest-tag)
  (bind (kern-mk-obj t_door 1) (list #f 0 dest-tag #f #f #f)))
