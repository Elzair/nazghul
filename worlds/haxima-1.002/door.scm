;;----------------------------------------------------------------------------
;; Doors
(define door-state-closed       0)
(define door-state-open         1)
(define door-state-locked       2)
(define door-state-magic-locked 3)

(define (mk-door-state sprite opacity pclass)
  (list sprite opacity pclass))
(define (door-state-sprite ds) (safe-eval (car ds)))
(define (door-state-opacity ds) (cadr ds))
(define (door-state-pclass ds) (caddr ds))

(define (mk-door-states closed open locked magic-locked)
  (list closed open locked magic-locked))

;; Define the door gob structure and procedures.
(define (door-mk open? timeout port active? locked? magic-locked? type)
  (list open? timeout port active? locked? magic-locked? type nil nil))
(define (door-open? door) (car (gob-data door)))
(define (door-timeout door) (cadr (gob-data door)))
(define (door-port door) (list-ref (gob-data door) 2))
(define (door-active? door) (list-ref (gob-data door) 3))
(define (door-locked? door) (list-ref (gob-data door) 4))
(define (door-magic-locked? door) (list-ref (gob-data door) 5))
(define (door-states door) (list-ref (gob-data door) 6))
(define (door-traps door) (list-ref (gob-data door) 7))
(define (door-trapped? door) (not (null? (door-traps door))))
(define (door-key door) (list-ref (gob-data door) 8))
(define (door-needs-key? door) (not (null? (door-key door))))
(define (door-key-fits? door ktype)
  (let ((key (safe-eval (door-key door))))
    (and (not (null? key))
         (eqv? key ktype))))

(define (door-set-open door val) (set-car! (gob-data door) val))
(define (door-set-timeout! door time) (set-car! (cdr (gob-data door)) time))
(define (door-set-port! door port) (set-car! (cddr (gob-data door)) port))
(define (door-set-active! door val) (set-car! (cdddr (gob-data door)) val))
(define (door-set-locked! door val) (set-car! (cddddr (gob-data door)) val))
(define (door-set-magic-locked! door val)
  (list-set-ref! (gob-data door) 5 val))
(define (door-set-traps! door val) (list-set-ref! (gob-data door) 7 val))
(define (door-add-trap! door trap-type)
  (door-set-traps! door (cons (mk-trap (eval trap-type))
                              (door-traps door))))
(define (door-set-key! door key-type-tag) 
  (list-set-ref! (gob-data door) 8 key-type-tag))

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
  (define (update state-no)
    (let ((state (list-ref (door-states (kobj-gob kdoor)) state-no)))
      (kern-obj-set-sprite kdoor (door-state-sprite state))
      (kern-obj-set-opacity kdoor (door-state-opacity state))
      (kern-obj-set-pclass kdoor (door-state-pclass state))))
  (let ((door (kobj-gob kdoor)))
    (cond ((door-magic-locked? door) (update door-state-magic-locked))
          ((door-locked? door)       (update door-state-locked))
          ((door-open? door)         (update door-state-open))
          (else                      (update door-state-closed))))
  (kern-map-set-dirty)
  kdoor)

(define (door-trip-traps kdoor kchar)
  (let ((door (kobj-gob kdoor))
        (thief-dice (string-append "1d" 
                                   (number->string (occ-ability-thief kchar))))
        )
    (kern-obj-inc-ref kdoor)
    (kern-obj-inc-ref kchar)
    (map (lambda (trap)
           (trap-trigger trap kdoor kchar))
         (door-traps door))
    (door-set-traps! door nil)
    (kern-obj-dec-ref kdoor)
    (kern-obj-dec-ref kchar)))

(define (door-open kdoor khandler) 
  (let ((door (kobj-gob kdoor)))
    (cond 
     ((door-magic-locked? door)
      (kern-log-msg "Magically locked!\n")
      #f)
     ((door-locked? door)
      (kern-log-msg "Locked!\n")
      #f)
     ((door-trapped? door)
      (door-trip-traps kdoor khandler)
      (door-open kdoor khandler)
      )
      (else
       (door-set-open door #t)
       (door-set-timeout! door 10)
       (door-update-kstate kdoor)
       (door-send-signal kdoor 'open)
       #t))))
  
(define (door-close2 kdoor khandler)
  ;;(display "door-close")(newline)
  (let ((door (kobj-gob kdoor)))
    (door-set-open door #f)
    (door-set-timeout! door 0)
    (door-update-kstate kdoor)
    (door-send-signal kdoor 'close)))

(define (door-close kdoor khandler)
  ;;(display "door-close")(newline)
  (if (not (occupied? (kern-obj-get-location kdoor)))
      (let ((door (kobj-gob kdoor)))
        (door-set-open door #f)
        (door-set-timeout! door 0)
        (door-update-kstate kdoor)
        (door-send-signal kdoor 'close))))

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
          ((door-needs-key? door) (kern-log-msg "Needs the key!\n"))
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


(define (door-connect kobj kto-tag)
  (let ((door (kobj-gob kobj)))
    (door-set-port! door kto-tag)))

(define (door-add-trap kdoor trap-sym)
  (let ((door (kobj-gob kdoor)))
    (door-add-trap! door trap-sym)))
  
(define (door-get-traps kdoor)
  (door-traps (kobj-gob kdoor)))

(define (door-rm-traps kdoor)
  (let ((door (kobj-gob kdoor)))
    (door-set-traps! door nil)))

(define (door-use-key kdoor key-type)
  (let ((door (kobj-gob kdoor)))
    (cond ((door-open? door) (kern-log-msg "Not closed!"))
          ((not (door-key-fits? door key-type)) (kern-log-msg "Key won't fit!"))
          ((door-locked? door)
           (door-set-locked! door #f)
           (door-update-kstate kdoor))
          (else
           (door-set-locked! door #t)
           (door-update-kstate kdoor)))))

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
       (method 'add-trap door-add-trap)
       (method 'get-traps door-get-traps)
       (method 'rm-traps door-rm-traps)
       (method 'use-key door-use-key)
       ))

;; Create the kernel "door" type
(mk-obj-type 't_door "door" s_closed_solid_wood_door_in_stone layer-mechanism 
             door-ifc)


;; Types for common door types
(define solid-wood-door-in-stone
  (mk-door-states
   (mk-door-state 's_closed_solid_wood_door_in_stone           #t pclass-wall)
   (mk-door-state 's_open_door_in_stone                        #f pclass-none)
   (mk-door-state 's_locked_solid_wood_door_in_stone           #t pclass-wall)
   (mk-door-state 's_magically_locked_solid_wood_door_in_stone #t pclass-wall)))

(define windowed-wood-door-in-rock
  (mk-door-states
   (mk-door-state 's_closed_windowed_wood_door_in_rock           #f pclass-wall)
   (mk-door-state 's_open_door_in_rock                           #f pclass-none)
   (mk-door-state 's_locked_windowed_wood_door_in_rock           #f pclass-wall)
   (mk-door-state 's_magically_locked_windowed_wood_door_in_rock #f pclass-wall)))

;;----------------------------------------------------------------------------
;; mk-door -- make and initialize a door object
;;
;; Used by the startup scripts when creating new doors.
;;
;;          type: one of the door state sets listed above
;;        locked: true iff door starts out locked
;;  magic-locked: true iff door starts out magically locked
;;  connected-to: nil, or the tag of an object the door forwards signals to
;;----------------------------------------------------------------------------
(define (mk-door-full type locked? magic-locked? connected-to)
  (bind (kern-mk-obj t_door 1)
        (door-mk #f 0 connected-to #f locked? magic-locked? type)))

;; Backward-compatible curried constructors
(define (mk-door) (mk-door-full solid-wood-door-in-stone #f #f nil))
(define (mk-locked-door) (mk-door-full solid-wood-door-in-stone #t #f nil))
(define (mk-connected-door tag)(mk-door-full solid-wood-door-in-stone #f #f tag))
(define (mk-windowed-door) (mk-door-full windowed-wood-door-in-rock #f #f nil))
(define (mk-magic-locked-door) (mk-door-full solid-wood-door-in-stone #f #t nil))
(define (mk-locked-windowed-door) 
  (mk-door-full windowed-wood-door-in-rock #t #f nil))

(define (lock-door-with-key kdoor key-type-tag)
  (lock-door kdoor nil)
  (door-set-key! (kobj-gob kdoor) key-type-tag)
  )
    

;; Add a trap to a door
(define (trap-door kdoor trap-tag)
  (ifccall kdoor 'add-trap trap-tag)
  kdoor
  )